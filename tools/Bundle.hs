{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Char (isAlphaNum, isLower)
import Data.List (nub, sort)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeExtension, (</>))

data ModuleInfo = ModuleInfo
  { moduleName :: String,
    moduleFilePath :: FilePath,
    moduleImports :: [String],
    modulePragmas :: [T.Text],
    topLevelDecls :: Map.Map String [T.Text],
    declDependencies :: Map.Map String (Set.Set String)
  }
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  let outputFile = case args of
        (file : _) -> file
        [] -> "submit.hs"

  putStrLn $ "Bundling to " ++ outputFile ++ " (auto-discovery mode)..."

  -- app/Main.hsを読み込み
  mainContent <- TIO.readFile "app/Main.hs"
  let mainInfo = parseModule "Main" "app/Main.hs" mainContent

  -- srcディレクトリ配下の全.hsファイルを自動検出
  srcModules <- discoverModules "src"

  putStrLn $ "Discovered " ++ show (length srcModules) ++ " modules in src/:"
  forM_ srcModules $ \(name, path) ->
    putStrLn $ "  - " ++ name ++ " (" ++ path ++ ")"

  -- 全モジュールを読み込み
  srcInfos <- forM srcModules $ \(name, path) -> do
    content <- TIO.readFile path
    return $ parseModule name path content

  let allModules =
        Map.fromList $
          ("Main", mainInfo) : [(moduleName m, m) | m <- srcInfos]

  -- mainから使われている関数を解析
  let usedFunctions = analyzeUsedFunctions allModules

  putStrLn $
    "\nTotal declarations: "
      ++ show (sum $ map (Map.size . topLevelDecls . snd) $ Map.toList allModules)
  putStrLn $ "Used declarations: " ++ show (Set.size usedFunctions)
  putStrLn "\nIncluded functions:"
  forM_ (Set.toList usedFunctions) $ \name ->
    putStrLn $ "  - " ++ name

  -- バンドル
  let bundled = bundleWithDeps allModules usedFunctions srcInfos
  TIO.writeFile outputFile bundled
  putStrLn "\n✓ Bundle complete!"

-- | srcディレクトリ配下の全.hsファイルを再帰的に探索
discoverModules :: FilePath -> IO [(String, FilePath)]
discoverModules dir = do
  entries <- listDirectory dir
  results <- forM entries $ \entry -> do
    let path = dir </> entry
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if isFile && takeExtension path == ".hs"
      then do
        -- モジュール名を推測: src/Data/Graph.hs → Data.Graph
        let modName = pathToModuleName dir path
        return [(modName, path)]
      else
        if isDir
          then discoverModules path
          else return []

  return $ concat results

-- | ファイルパスからモジュール名を生成
-- 例: src/Data/Graph.hs → Data.Graph
pathToModuleName :: FilePath -> FilePath -> String
pathToModuleName baseDir filePath =
  let relative = drop (length baseDir + 1) filePath -- "Data/Graph.hs"
      withoutExt = dropExtension relative -- "Data/Graph"
      modName = map (\c -> if c == '/' then '.' else c) withoutExt
   in modName

-- | LANGUAGEプラグマを抽出
extractPragmas :: [T.Text] -> [T.Text]
extractPragmas = mapMaybe extractPragma
  where
    extractPragma line
      | "{-# LANGUAGE " `T.isPrefixOf` T.stripStart line = Just line
      | otherwise = Nothing

parseModule :: String -> FilePath -> T.Text -> ModuleInfo
parseModule name path content =
  let lines' = T.lines content
      pragmas = extractPragmas lines'
      imports = extractImports lines'
      decls = extractDeclarations lines'
      deps = Map.map (extractDeps decls) decls
   in ModuleInfo name path imports pragmas decls deps

extractImports :: [T.Text] -> [String]
extractImports = mapMaybe parseImport
  where
    parseImport line
      | "import " `T.isPrefixOf` T.stripStart line =
          let parts = T.words line
           in case parts of
                ["import", "qualified", modName, "as", _] -> Just (T.unpack modName)
                ["import", "qualified", modName] -> Just (T.unpack modName)
                ["import", modName] -> Just (T.unpack modName)
                ("import" : modName : _) -> Just (T.unpack modName)
                _ -> Nothing
      | otherwise = Nothing

extractDeclarations :: [T.Text] -> Map.Map String [T.Text]
extractDeclarations lines' =
  let numbered = zip [0 ..] lines'
      bodyStart = findBodyStart numbered
      bodyLines = drop bodyStart numbered
      grouped = groupDeclarations bodyLines
   in Map.fromList $ map (\g -> (getDeclName g, map snd g)) grouped
  where
    findBodyStart [] = 0
    findBodyStart ((i, line) : rest)
      | shouldSkipLine line = findBodyStart rest
      | otherwise = i
    shouldSkipLine line =
      let s = T.stripStart line
       in T.isPrefixOf "module " s
            || T.isPrefixOf "import " s
            || T.isPrefixOf "{-# " s
            || T.null s

groupDeclarations :: [(Int, T.Text)] -> [[(Int, T.Text)]]
groupDeclarations [] = []
groupDeclarations ((i, line) : rest)
  | isTopLevel line =
      let (group, remaining) = span (not . isTopLevel . snd) rest
       in ((i, line) : group) : groupDeclarations remaining
  | otherwise = groupDeclarations rest
  where
    isTopLevel line =
      let s = T.stripStart line
       in not (T.null s)
            && not (T.isPrefixOf " " line)
            && not (T.isPrefixOf "\t" line)
            && not (T.isPrefixOf "--" s)

getDeclName :: [(Int, T.Text)] -> String
getDeclName [] = "unknown"
getDeclName ((_, line) : _) =
  let firstWord = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') $ T.stripStart line
   in T.unpack $ T.takeWhile (/= ':') firstWord

extractDeps :: Map.Map String [T.Text] -> [T.Text] -> Set.Set String
extractDeps allDecls lines' =
  let text = T.unlines lines'
      identifiers = extractIdentifiers text
      localFuncs = Set.fromList $ Map.keys allDecls
   in Set.intersection identifiers localFuncs

extractIdentifiers :: T.Text -> Set.Set String
extractIdentifiers text =
  let words' = T.words text
      identifiers = filter isValidIdentifier $ map T.unpack words'
   in Set.fromList identifiers
  where
    isValidIdentifier [] = False
    isValidIdentifier (c : rest) =
      isLower c && all (\x -> isAlphaNum x || x == '_' || x == '\'') rest

analyzeUsedFunctions :: Map.Map String ModuleInfo -> Set.Set String
analyzeUsedFunctions modules = go Set.empty (Set.singleton "main")
  where
    go visited toVisit
      | Set.null toVisit = visited
      | otherwise =
          let (current, rest) = Set.deleteFindMin toVisit
           in if Set.member current visited
                then go visited rest
                else
                  let newVisited = Set.insert current visited
                      deps = findDeps current modules
                      newToVisit = Set.union rest (Set.difference deps visited)
                   in go newVisited newToVisit

findDeps :: String -> Map.Map String ModuleInfo -> Set.Set String
findDeps funcName modules =
  let allDeps = mapMaybe (Map.lookup funcName . declDependencies) (Map.elems modules)
   in case allDeps of (deps : _) -> deps; [] -> Set.empty

bundleWithDeps :: Map.Map String ModuleInfo -> Set.Set String -> [ModuleInfo] -> T.Text
bundleWithDeps modules usedFunctions srcModules =
  T.unlines $
    collectAllPragmas modules
      ++ [""]
      ++ collectExternalImports modules
      ++ [""]
      ++
      -- srcモジュールを順番に出力
      concatMap
        ( \info ->
            let name = moduleName info
             in if name /= "Main" -- Mainは最後
                  then
                    ["-- ====== " `T.append` T.pack name `T.append` " ======", ""]
                      ++ emitUsedDecls name modules usedFunctions
                      ++ [""]
                  else []
        )
        srcModules
      ++ ["-- ====== Main ======", ""]
      ++ emitUsedDecls "Main" modules usedFunctions

-- | 全モジュールからプラグマを収集（重複削除）
collectAllPragmas :: Map.Map String ModuleInfo -> [T.Text]
collectAllPragmas modules =
  let allPragmas = concatMap (modulePragmas . snd) (Map.toList modules)
      uniquePragmas = nub $ sort allPragmas
   in uniquePragmas

-- | 全モジュールから外部importを抽出
collectExternalImports :: Map.Map String ModuleInfo -> [T.Text]
collectExternalImports modules =
  let allImports = concatMap (moduleImports . snd) (Map.toList modules)
      -- 内部モジュール名のリスト
      internalModules = Set.fromList $ Map.keys modules
      -- 外部モジュールのみ
      externalOnly = filter (\m -> not (Set.member m internalModules)) allImports
   in map (T.pack . ("import " ++)) $ nub $ sort externalOnly

emitUsedDecls :: String -> Map.Map String ModuleInfo -> Set.Set String -> [T.Text]
emitUsedDecls modName modules usedFunctions =
  case Map.lookup modName modules of
    Nothing -> []
    Just info ->
      let allDecls = topLevelDecls info
          usedDecls = Map.filterWithKey (\name _ -> Set.member name usedFunctions) allDecls
       in concatMap (\(_, lines') -> lines' ++ [""]) $ Map.toList usedDecls