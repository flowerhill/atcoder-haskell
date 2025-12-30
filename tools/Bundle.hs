{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Char (isAlphaNum, isLower)
import Data.List (nub, nubBy, partition, sort, sortOn)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeExtension, (</>))

data ModuleInfo = ModuleInfo
  { moduleName :: String,
    moduleFilePath :: FilePath,
    moduleImports :: [(String, Maybe String)],
    modulePragmas :: [T.Text],
    declarations :: Map.Map String [T.Text],
    declDeps :: Map.Map String (Set.Set String)
  }
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  let outputFile = case args of
        (file : _) -> file
        [] -> "dist/Submit.hs"

  createDirectoryIfMissing True "dist"
  putStrLn $ "Bundling to " ++ outputFile ++ "..."

  mainContent <- TIO.readFile "app/Main.hs"
  let mainInfo = parseModule "Main" "app/Main.hs" mainContent

  putStrLn "\n=== Main module ==="
  putStrLn $ "Declarations: " ++ show (Map.keys (declarations mainInfo))
  case Map.lookup "main" (declarations mainInfo) of
    Nothing -> putStrLn "ERROR: 'main' declaration not found!"
    Just mainDecl -> do
      putStrLn "\nmain declaration:"
      mapM_ (putStrLn . ("  " ++) . T.unpack) mainDecl

  srcModules <- discoverModules "src"
  putStrLn $ "Discovered " ++ show (length srcModules) ++ " modules:"
  forM_ srcModules $ \(name, _) -> putStrLn $ "  - " ++ name

  srcInfos <- forM srcModules $ \(name, path) -> do
    content <- TIO.readFile path
    let modInfo = parseModule name path content
    putStrLn $ "\n  Module: " ++ name
    putStrLn $ "    Declarations: " ++ show (Map.keys (declarations modInfo))
    return modInfo

  let allModules =
        Map.fromList $
          ("Main", mainInfo) : [(moduleName m, m) | m <- srcInfos]

  -- 全モジュールの関数名を収集
  let allFunctionNames = Set.unions [Map.keysSet (declarations m) | m <- Map.elems allModules]

  -- 各モジュールの依存関係を全関数名で計算
  let modulesWithDeps =
        Map.map
          ( \modInfo ->
              let newDeps = Map.map (extractDepsGlobal allFunctionNames) (declarations modInfo)
               in modInfo {declDeps = newDeps}
          )
          allModules

  -- デバッグ: mainの依存関係を表示
  case Map.lookup "Main" modulesWithDeps of
    Nothing -> putStrLn "\nWarning: Main module not found"
    Just mainMod -> case Map.lookup "main" (declDeps mainMod) of
      Nothing -> putStrLn "\nWarning: 'main' declaration not found"
      Just deps -> do
        putStrLn $ "\nmain raw dependencies: " ++ show (Set.toList deps)

  let usedFunctions = analyzeUsedFunctions "main" modulesWithDeps

  putStrLn $
    "\nTotal functions: "
      ++ show (sum [Map.size (declarations m) | m <- Map.elems allModules])
  putStrLn $ "Used functions: " ++ show (Set.size usedFunctions)
  putStrLn "\nIncluded functions:"
  forM_ (take 20 $ Set.toList usedFunctions) $ \name ->
    putStrLn $ "  - " ++ name

  let bundled = functionLevelBundle modulesWithDeps usedFunctions srcInfos
  TIO.writeFile outputFile bundled
  putStrLn "\n✓ Complete!"

discoverModules :: FilePath -> IO [(String, FilePath)]
discoverModules dir = do
  entries <- listDirectory dir
  results <- forM entries $ \entry -> do
    let path = dir </> entry
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    if isFile && takeExtension path == ".hs"
      then return [(pathToModuleName dir path, path)]
      else
        if isDir
          then discoverModules path
          else return []
  return $ concat results

pathToModuleName :: FilePath -> FilePath -> String
pathToModuleName baseDir filePath =
  let relative = drop (length baseDir + 1) filePath
      withoutExt = dropExtension relative
   in map (\c -> if c == '/' then '.' else c) withoutExt

parseModule :: String -> FilePath -> T.Text -> ModuleInfo
parseModule name path content =
  let lines' = T.lines content
      pragmas = extractPragmas lines'
      imports = extractImports lines'
      decls = extractDeclarations lines'
      -- 依存関係は後でまとめて計算するため、空にしておく
      deps = Map.empty
   in ModuleInfo name path imports pragmas decls deps

extractPragmas :: [T.Text] -> [T.Text]
extractPragmas =
  filter
    ( \line ->
        let s = T.stripStart line
         in "{-# LANGUAGE " `T.isPrefixOf` s
    )

extractImports :: [T.Text] -> [(String, Maybe String)]
extractImports = mapMaybe parseImport
  where
    parseImport line
      | "import " `T.isPrefixOf` T.stripStart line =
          let parts = T.words line
           in case parts of
                ["import", "qualified", modName, "as", alias] ->
                  Just (T.unpack modName, Just (T.unpack alias))
                ["import", "qualified", modName] ->
                  Just (T.unpack modName, Nothing)
                ["import", modName] ->
                  Just (T.unpack modName, Nothing)
                ("import" : modName : _) ->
                  Just (T.unpack modName, Nothing)
                _ -> Nothing
      | otherwise = Nothing

extractDeclarations :: [T.Text] -> Map.Map String [T.Text]
extractDeclarations lines' =
  let bodyLines = dropWhile isHeaderLine lines'
      groups = splitIntoDeclarations bodyLines
   in Map.fromList [(name, decl) | (name, decl) <- groups, not (null name)]
  where
    isHeaderLine line =
      let s = T.stripStart line
       in T.null s
            || T.isPrefixOf "module " s
            || T.isPrefixOf "{-#" s
            || T.isPrefixOf "import " s -- 全てのpragmaをスキップ
            || T.isPrefixOf "--" s -- コメント行もスキップ

splitIntoDeclarations :: [T.Text] -> [(String, [T.Text])]
splitIntoDeclarations lines' = go (dropWhile isEmpty lines')
  where
    isEmpty line = T.null (T.strip line)

    go [] = []
    go (line : rest)
      | isEmpty line = go rest
      | isComment line = go rest
      | isTopLevel line =
          let name = extractName line
              (declLines, remaining) = takeUntilNextDecl rest
           in if null name
                then go remaining
                else (name, line : declLines) : go remaining
      | otherwise = go rest

    isTopLevel line =
      not (T.null line)
        && not (T.isPrefixOf " " line)
        && not (T.isPrefixOf "\t" line)

    isComment line = "--" `T.isPrefixOf` T.stripStart line

    takeUntilNextDecl [] = ([], [])
    takeUntilNextDecl (line : rest)
      | isEmpty line =
          let (more, remaining) = takeUntilNextDecl rest
           in (line : more, remaining)
      | isTopLevel line = ([], line : rest)
      | otherwise =
          let (more, remaining) = takeUntilNextDecl rest
           in (line : more, remaining)

    extractName line =
      let stripped = T.strip line
          -- 型シグネチャかどうかチェック
          hasTypeSign = T.isInfixOf "::" stripped
          -- "::" の前または空白の前までを名前として取得
          beforeTypeSign =
            if hasTypeSign
              then T.takeWhile (/= ':') stripped
              else T.takeWhile (\c -> c /= ' ' && c /= '=') stripped
          name = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') beforeTypeSign
       in T.unpack $ T.strip name

extractDeps :: Map.Map String [T.Text] -> [T.Text] -> Set.Set String
extractDeps allDecls lines' =
  let text = T.unlines lines'
      identifiers = extractIdentifiers text
      declNames = Map.keysSet allDecls
      result = Set.intersection identifiers declNames
   in result

extractDepsGlobal :: Set.Set String -> [T.Text] -> Set.Set String
extractDepsGlobal allFunctionNames lines' =
  let text = T.unlines lines'
      identifiers = extractIdentifiers text
   in Set.intersection identifiers allFunctionNames

extractIdentifiers :: T.Text -> Set.Set String
extractIdentifiers text =
  -- 全ての単語を抽出（記号で分割）
  let allTokens = T.split (\c -> not (isAlphaNum c || c == '_' || c == '\'')) text
      -- 小文字で始まる識別子だけを抽出
      identifiers = [T.unpack token | token <- allTokens, isValidIdent token]
   in Set.fromList identifiers
  where
    isValidIdent t =
      case T.uncons t of
        Nothing -> False
        Just (c, _) -> isLower c && T.length t > 0

analyzeUsedFunctions :: String -> Map.Map String ModuleInfo -> Set.Set String
analyzeUsedFunctions startFunc allModules =
  go Set.empty (Set.singleton startFunc) Set.empty
  where
    go visited toVisit processedFuncs
      | Set.null toVisit = visited
      | otherwise =
          let (current, rest) = Set.deleteFindMin toVisit
           in if Set.member current visited
                then go visited rest processedFuncs
                else
                  let newVisited = Set.insert current visited
                      -- 依存関係を取得（複数のモジュールに同名関数がある場合は最初の1つだけ）
                      deps = case findDepsOnce current allModules processedFuncs of
                        (Just newDeps, srcModule) ->
                          let newProcessedFuncs = Set.insert (srcModule, current) processedFuncs
                           in (newDeps, newProcessedFuncs)
                        (Nothing, _) -> (Set.empty, processedFuncs)
                      newToVisit = Set.union rest (Set.difference (fst deps) newVisited)
                   in go newVisited newToVisit (snd deps)

-- 関数を最初に見つけたモジュールだけから取得
findDepsOnce :: String -> Map.Map String ModuleInfo -> Set.Set (String, String) -> (Maybe (Set.Set String), String)
findDepsOnce funcName allModules processedFuncs =
  let candidates =
        [ (Map.lookup funcName (declDeps info), moduleName info)
          | info <- Map.elems allModules,
            Map.member funcName (declarations info),
            not (Set.member (moduleName info, funcName) processedFuncs)
        ]
   in case candidates of
        ((Just deps, modName) : _) -> (Just deps, modName)
        _ -> (Nothing, "")

functionLevelBundle :: Map.Map String ModuleInfo -> Set.Set String -> [ModuleInfo] -> T.Text
functionLevelBundle allModules usedFunctions srcInfos =
  let usedModules =
        [ m | m <- Map.elems allModules, any (`Set.member` usedFunctions) (Map.keys (declarations m))
        ]
      neededImports = collectImportsFromModules usedModules

      allFunctions =
        concatMap (emitUsedFunctions usedFunctions) srcInfos
          ++ case Map.lookup "Main" allModules of
            Nothing -> []
            Just mainInfo -> emitUsedFunctions usedFunctions mainInfo

      -- 連続する空行を1つにまとめる
      cleanedFunctions = removeExcessiveBlankLines allFunctions
   in T.unlines $
        collectAllPragmas allModules
          ++ [""]
          ++ neededImports
          ++ (["" | not (null neededImports)])
          ++ cleanedFunctions

-- 連続する空行を1つにまとめる
removeExcessiveBlankLines :: [T.Text] -> [T.Text]
removeExcessiveBlankLines [] = []
removeExcessiveBlankLines [x] = [x]
removeExcessiveBlankLines (x : y : rest)
  | T.null (T.strip x) && T.null (T.strip y) = removeExcessiveBlankLines (x : rest)
  | otherwise = x : removeExcessiveBlankLines (y : rest)

emitUsedFunctions :: Set.Set String -> ModuleInfo -> [T.Text]
emitUsedFunctions usedFunctions modInfo =
  let allDecls = declarations modInfo
      usedDecls = Map.filterWithKey (\name _ -> Set.member name usedFunctions) allDecls
   in if Map.null usedDecls
        then []
        else concatMap snd (Map.toList usedDecls)

collectAllPragmas :: Map.Map String ModuleInfo -> [T.Text]
collectAllPragmas modules =
  nub $ sort $ concatMap (modulePragmas . snd) (Map.toList modules)

collectImportsFromModules :: [ModuleInfo] -> [T.Text]
collectImportsFromModules modules =
  let -- 全てのimportを収集（重複あり）
      allImports = concatMap moduleImports modules
      -- 内部モジュール名を収集
      internalNames = Set.fromList [moduleName m | m <- modules]
      -- 外部モジュールのみ
      externalOnly = filter (\(modName, _) -> not (Set.member modName internalNames)) allImports
      -- (モジュール名, エイリアス)のペアで重複削除
      -- 同じモジュールが複数回importされていても1回だけ含める
      uniqueImports = nubBy (\(m1, a1) (m2, a2) -> m1 == m2 && a1 == a2) externalOnly
      -- qualified と non-qualified を分離してソート
      (qualified, nonQualified) = partition (\(_, alias) -> isJust alias) uniqueImports
      sortedImports = sortOn fst nonQualified ++ sortOn fst qualified
   in map formatImport sortedImports
  where
    formatImport (modName, Nothing) = T.pack $ "import " ++ modName
    formatImport (modName, Just alias) = T.pack $ "import qualified " ++ modName ++ " as " ++ alias

collectExternalImports modules =
  let allImports = concatMap (moduleImports . snd) (Map.toList modules)
      internalModules = Map.keysSet modules
      externalOnly = filter (\(m, _) -> not (Set.member m internalModules)) allImports
      uniqueImports = nub $ sort externalOnly
      (qualified, nonQualified) = partition (\(_, alias) -> isJust alias) uniqueImports
   in map formatImport (nonQualified ++ qualified)
  where
    formatImport (modName, Nothing) = T.pack $ "import " ++ modName
    formatImport (modName, Just alias) = T.pack $ "import qualified " ++ modName ++ " as " ++ alias