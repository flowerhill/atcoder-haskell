{-# LANGUAGE OverloadedStrings #-}

-- | Haskell Module Bundler for AtCoder
-- 複数のHaskellモジュールを1つのMain.hsにバンドルする
--
-- Usage: cabal run bundle [src-dir] <Main.hs>
--
-- Example:
--  cabal run bundle src app/Main.hs > submit/Main.hs
module Main where

import Control.Monad (forM)
import Data.Char (isSpace)
import Data.List (isPrefixOf, nub, partition)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (IOMode (..), hGetContents, hSetEncoding, openFile, utf8)

-- | UTF-8でファイルを読み込む
readFileUtf8 :: FilePath -> IO String
readFileUtf8 path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  content <- hGetContents h
  -- 遅延評価を強制
  length content `seq` return content

-- | モジュール情報
data ModuleInfo = ModuleInfo
  { modName :: String, -- モジュール名 (e.g., "Solve", "Lib.Utils")
    modPath :: FilePath, -- ファイルパス
    modImports :: [ImportInfo], -- import文
    modExports :: Maybe String, -- export list (括弧内)
    modPragmas :: [String], -- LANGUAGE プラグマ
    modBody :: [String] -- モジュール本体 (import以降)
  }
  deriving (Show)

-- | import情報
data ImportInfo = ImportInfo
  { impQualified :: Bool,
    impName :: String,
    impAs :: Maybe String,
    impHiding :: Bool,
    impList :: Maybe String, -- import list
    impRaw :: String -- 元の行
  }
  deriving (Show, Eq)

-- | メイン処理
main :: IO ()
main = do
  args <- getArgs
  case args of
    [srcDir, mainFile] -> bundle srcDir mainFile
    [mainFile] -> bundle "." mainFile
    _ -> putStrLn "Usage: hs-bundler [src-dir] <Main.hs>"

-- | バンドル処理
bundle :: FilePath -> FilePath -> IO ()
bundle srcDir mainFile = do
  mainContent <- readFileUtf8 mainFile
  let mainInfo = parseModule "Main" mainFile mainContent

  localModules <- collectLocalModules srcDir mainInfo S.empty
  let sorted = topSort localModules
  let output = generateBundle mainInfo sorted localModules
  putStrLn output

-- | ローカルモジュールを再帰的に収集
collectLocalModules :: FilePath -> ModuleInfo -> S.Set String -> IO (M.Map String ModuleInfo)
collectLocalModules srcDir modInfo visited = do
  let localImports = filter (isLocalImport srcDir) (modImports modInfo)
      newNames = filter (`S.notMember` visited) (map impName localImports)
      visited' = foldr S.insert visited newNames

  -- 各ローカルモジュールを読み込む
  mods <- forM newNames $ \name -> do
    let path = srcDir </> moduleToPath name
    exists <- doesFileExist path
    if exists
      then do
        content <- readFileUtf8 path
        let info = parseModule name path content
        subMods <- collectLocalModules srcDir info visited'
        return $ M.insert name info subMods
      else return M.empty

  return $ M.unions mods

-- | モジュール名からファイルパスへ変換
moduleToPath :: String -> FilePath
moduleToPath name = map (\c -> if c == '.' then '/' else c) name ++ ".hs"

-- | ローカルモジュールかどうか判定
isLocalImport :: FilePath -> ImportInfo -> Bool
isLocalImport _ imp = not (isStandardModule (impName imp))

-- | 標準/外部ライブラリモジュールかどうか
isStandardModule :: String -> Bool
isStandardModule name = any (`isPrefixOf` name) standardPrefixes
  where
    standardPrefixes =
      [ "Prelude",
        "Data.",
        "Control.",
        "System.",
        "Text.",
        "GHC.",
        "Foreign.",
        "Numeric.",
        "Debug.",
        "Unsafe.",
        "Type.",
        -- 競プロでよく使う外部ライブラリ
        "qualified" -- パースミス対策
      ]

-- | モジュールをパース
parseModule :: String -> FilePath -> String -> ModuleInfo
parseModule name path content =
  ModuleInfo
    { modName = actualName,
      modPath = path,
      modImports = imports,
      modExports = exports,
      modPragmas = pragmas,
      modBody = body
    }
  where
    lns = lines content
    pragmas = extractPragmas lns
    (actualName, exports, afterModule) = extractModuleHeader lns
    (importLines, body) = extractImportsAndBody afterModule
    imports = map parseImport importLines

-- | プラグマを抽出（LANGUAGE, OPTIONS_GHC など全て）
extractPragmas :: [String] -> [String]
extractPragmas = filter isPragma
  where
    isPragma l =
      "{-#" `isPrefixOf` dropWhile isSpace l

-- | モジュールヘッダーを抽出
extractModuleHeader :: [String] -> (String, Maybe String, [String])
extractModuleHeader lns =
  case findModuleLine lns of
    Nothing -> ("Main", Nothing, lns)
    Just (modLine, rest) ->
      let (name, exports) = parseModuleLine modLine
          -- module宣言がwhereで終わっていなければ、whereまで読み飛ばす
          rest' =
            if "where" `isSuffixOf` dropWhileEnd isSpace modLine
              then rest
              else dropUntilWhere rest
       in (name, exports, rest')

-- | whereが出てくるまで読み飛ばす
dropUntilWhere :: [String] -> [String]
dropUntilWhere [] = []
dropUntilWhere (l : ls)
  | "where" `isSuffixOf` dropWhileEnd isSpace l = ls
  | trimmed == "where" = ls
  | otherwise = dropUntilWhere ls
  where
    trimmed = dropWhile isSpace l

-- | 末尾から条件を満たす要素を削除
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x acc -> if null acc && p x then [] else x : acc) []

-- | 末尾一致判定
isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf suffix xs = suffix == drop (length xs - length suffix) xs

-- | module行を見つける
findModuleLine :: [String] -> Maybe (String, [String])
findModuleLine [] = Nothing
findModuleLine (l : ls)
  | "module " `isPrefixOf` stripped = Just (l, ls)
  | otherwise = findModuleLine ls
  where
    stripped = dropWhile isSpace l

-- | module行をパース
parseModuleLine :: String -> (String, Maybe String)
parseModuleLine l =
  let stripped = dropWhile isSpace l
      afterModule = dropWhile isSpace $ drop 7 stripped -- "module " を除去
      (name, rest) = span (\c -> not (isSpace c) && c /= '(') afterModule
      exports = extractExportList rest
   in (name, exports)

-- | export listを抽出
extractExportList :: String -> Maybe String
extractExportList s =
  let trimmed = dropWhile isSpace s
   in if null trimmed || head trimmed /= '('
        then Nothing
        else Just $ takeParens trimmed

-- | 括弧の対応を取って抽出
takeParens :: String -> String
takeParens = go 0
  where
    go _ [] = []
    go n (c : cs)
      | c == '(' = c : go (n + 1) cs
      | c == ')' = if n == 1 then [c] else c : go (n - 1) cs
      | otherwise = c : go n cs

-- | import文と本体を分離
extractImportsAndBody :: [String] -> ([String], [String])
extractImportsAndBody lns =
  let (imports, body) = partition isImportLine lns
   in (imports, filter (not . isSkipLine) body)
  where
    isImportLine l =
      let trimmed = dropWhile isSpace l
       in "import " `isPrefixOf` trimmed
    isSkipLine l =
      let trimmed = dropWhile isSpace l
       in null trimmed
            || "module " `isPrefixOf` trimmed

-- | import文をパース
parseImport :: String -> ImportInfo
parseImport l =
  ImportInfo
    { impQualified = "qualified" `elem` ws,
      impName = extractModuleName ws,
      impAs = extractAs ws,
      impHiding = "hiding" `elem` ws,
      impList = extractImportList l,
      impRaw = l
    }
  where
    ws = words l

-- | import文からモジュール名を抽出
extractModuleName :: [String] -> String
extractModuleName ws =
  case dropWhile (\w -> w `elem` ["import", "qualified"]) ws of
    (name : _) -> takeWhile (\c -> c /= '(' && not (isSpace c)) name
    [] -> ""

-- | as句を抽出
extractAs :: [String] -> Maybe String
extractAs ws =
  case dropWhile (/= "as") ws of
    (_ : name : _) -> Just name
    _ -> Nothing

-- | import listを抽出
extractImportList :: String -> Maybe String
extractImportList l =
  let idx = findImportListStart l
   in if idx >= 0
        then Just $ drop idx l
        else Nothing
  where
    findImportListStart s =
      case break (== '(') s of
        (_, "") -> -1
        (before, _) -> length before

-- | トポロジカルソート
topSort :: M.Map String ModuleInfo -> [ModuleInfo]
topSort mods = reverse $ go S.empty [] (M.keys mods)
  where
    go _ acc [] = acc
    go visited acc (name : rest)
      | name `S.member` visited = go visited acc rest
      | otherwise =
          case M.lookup name mods of
            Nothing -> go visited acc rest
            Just info ->
              let deps = filter (`M.member` mods) (map impName $ modImports info)
                  visited' = S.insert name visited
                  acc' = go visited' acc deps
               in go visited' (info : acc') rest

-- | バンドル出力を生成
generateBundle :: ModuleInfo -> [ModuleInfo] -> M.Map String ModuleInfo -> String
generateBundle mainMod localMods localModMap =
  unlines $
    -- 1. 全プラグマを集約
    nub (concatMap modPragmas (mainMod : localMods))
      ++ [""]
      ++
      -- 2. module Main where
      ["module Main where", ""]
      ++
      -- 3. 外部ライブラリのimportのみ残す
      externalImports
      ++ [""]
      ++
      -- 4. ローカルモジュールの本体を展開
      concatMap expandModule localMods
      ++
      -- 5. Main本体
      ["", "-- Main"]
      ++ modBody mainMod
  where
    allImports = concatMap modImports (mainMod : localMods)
    localNames = S.fromList $ "Main" : M.keys localModMap
    externalImports =
      nub
        [ impRaw imp
          | imp <- allImports,
            impName imp `S.notMember` localNames,
            not (null (impName imp))
        ]

-- | モジュールを展開
expandModule :: ModuleInfo -> [String]
expandModule info =
  ["", "-- " ++ modName info] ++ modBody info