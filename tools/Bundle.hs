{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Haskell Module Bundler for AtCoder
-- 複数のHaskellモジュールを1つのMain.hsにバンドルする
--
-- Usage: cabal run bundle [src-dir] <Main.hs>
--
-- Example:
--  cabal run bundle src app/Main.hs > submit/Main.hs
module Main where

import Control.Monad (forM)
import Data.Char (isAlphaNum, isSpace)
import Data.List (intercalate, isPrefixOf, nub)
import Data.Maybe (fromMaybe)
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
    modPragmas :: [String], -- LANGUAGE プラグマ（先頭に集約するもの）
    modBody :: [String] -- モジュール本体 (import以降、INLINEプラグマ含む)
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
  let mainInfo = parseModule mainFile mainContent

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
        let info = parseModule path content
        subMods <- collectLocalModules srcDir info visited'
        return $ M.insert name info subMods
      else return M.empty

  return $ M.unions mods

-- | モジュール名からファイルパスへ変換
--
-- >>> moduleToPath "Lib"
-- "Lib.hs"
-- >>> moduleToPath "Data.List"
-- "Data/List.hs"
-- >>> moduleToPath "A.B.C"
-- "A/B/C.hs"
moduleToPath :: String -> FilePath
moduleToPath name = map (\c -> if c == '.' then '/' else c) name ++ ".hs"

-- | ローカルモジュールかどうか判定
--
-- >>> isLocalImport "" (parseImport "import Data.List")
-- False
-- >>> isLocalImport "" (parseImport "import Lib")
-- True
-- >>> isLocalImport "" (parseImport "import qualified MyModule as MM")
-- True
isLocalImport :: FilePath -> ImportInfo -> Bool
isLocalImport _ imp = not (isStandardModule (impName imp))

-- | 標準/外部ライブラリモジュールかどうか
--
-- >>> isStandardModule "Data.List"
-- True
-- >>> isStandardModule "Control.Monad.ST"
-- True
-- >>> isStandardModule "Lib"
-- False
-- >>> isStandardModule "MyModule"
-- False
-- >>> isStandardModule "Prelude"
-- True
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
--
-- >>> modName (parseModule "Foo.hs" "module Foo where\nx = 1")
-- "Foo"
-- >>> modPath (parseModule "Foo.hs" "module Foo where")
-- "Foo.hs"
-- >>> modName (parseModule "Bar.hs" "x = 1")
-- "Main"
-- >>> map impName (modImports (parseModule "F.hs" "module F where\nimport Data.List"))
-- ["Data.List"]
-- >>> modBody (parseModule "F.hs" "module F where\nfoo = 1")
-- ["foo = 1"]
parseModule :: FilePath -> String -> ModuleInfo
parseModule path content =
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
    pragmas = extractTopLevelPragmas lns -- 先頭に集約するプラグマのみ
    (actualName, exports, afterModule) = extractModuleHeader lns
    (importLines, body) = extractImportsAndBody afterModule
    imports = map parseImport importLines

-- | 先頭に集約すべきプラグマを抽出（LANGUAGE, OPTIONS_GHC のみ）
--
-- >>> extractTopLevelPragmas ["{-# LANGUAGE BangPatterns #-}", "module Foo where"]
-- ["{-# LANGUAGE BangPatterns #-}"]
-- >>> extractTopLevelPragmas ["{-# INLINE foo #-}", "{-# LANGUAGE CPP #-}"]
-- ["{-# LANGUAGE CPP #-}"]
-- >>> extractTopLevelPragmas []
-- []
extractTopLevelPragmas :: [String] -> [String]
extractTopLevelPragmas = filter isTopLevelPragma
  where
    isTopLevelPragma l =
      let trimmed = dropWhile isSpace l
       in isPragmaType "LANGUAGE" trimmed
            || isPragmaType "OPTIONS_GHC" trimmed
            || isPragmaType "OPTIONS" trimmed

-- | 特定のプラグマタイプかどうか判定
--
-- >>> isPragmaType "LANGUAGE" "{-# LANGUAGE BangPatterns #-}"
-- True
-- >>> isPragmaType "INLINE" "{-# LANGUAGE BangPatterns #-}"
-- False
-- >>> isPragmaType "OPTIONS_GHC" "{-# OPTIONS_GHC -Wall #-}"
-- True
-- >>> isPragmaType "LANGUAGE" "x = 1"
-- False
isPragmaType :: String -> String -> Bool
isPragmaType pragmaName l =
  "{-#" `isPrefixOf` l
    && (pragmaName `isPrefixOf` dropWhile isSpace (drop 3 l))

-- | 関数に紐づくプラグマかどうか（本体に残すべきもの）
--
-- >>> isFunctionPragma "{-# INLINE foo #-}"
-- True
-- >>> isFunctionPragma "{-# LANGUAGE BangPatterns #-}"
-- False
-- >>> isFunctionPragma "  {-# SPECIALIZE foo @Int #-}"
-- True
-- >>> isFunctionPragma "foo = 1"
-- False
isFunctionPragma :: String -> Bool
isFunctionPragma l =
  let trimmed = dropWhile isSpace l
   in "{-#" `isPrefixOf` trimmed
        && ( isPragmaType "INLINE" trimmed
               || isPragmaType "NOINLINE" trimmed
               || isPragmaType "INLINABLE" trimmed
               || isPragmaType "SPECIALIZE" trimmed
               || isPragmaType "SPECIALISE" trimmed
               || isPragmaType "RULES" trimmed
               || isPragmaType "ANN" trimmed
               || isPragmaType "UNPACK" trimmed
               || isPragmaType "NOUNPACK" trimmed
               || isPragmaType "COMPLETE" trimmed
           )

-- | モジュールヘッダーを抽出
--
-- >>> extractModuleHeader ["module Foo where", "x = 1"]
-- ("Foo",Nothing,["x = 1"])
-- >>> extractModuleHeader ["module Foo (bar) where", "x = 1"]
-- ("Foo",Just "(bar)",["x = 1"])
-- >>> extractModuleHeader ["x = 1"]
-- ("Main",Nothing,["x = 1"])
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
--
-- >>> dropUntilWhere ["a", "b where", "c"]
-- ["c"]
-- >>> dropUntilWhere ["where", "x"]
-- ["x"]
-- >>> dropUntilWhere []
-- []
dropUntilWhere :: [String] -> [String]
dropUntilWhere [] = []
dropUntilWhere (l : ls)
  | "where" `isSuffixOf` dropWhileEnd isSpace l = ls
  | trimmed == "where" = ls
  | otherwise = dropUntilWhere ls
  where
    trimmed = dropWhile isSpace l

-- | 末尾から条件を満たす要素を削除
--
-- >>> dropWhileEnd (== ' ') "hello   "
-- "hello"
-- >>> dropWhileEnd (== ' ') ""
-- ""
-- >>> dropWhileEnd (> 3) [1,2,3,4,5]
-- [1,2,3]
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x acc -> if null acc && p x then [] else x : acc) []

-- | 末尾一致判定
--
-- >>> "ld" `isSuffixOf` "hello world"
-- True
-- >>> "foo" `isSuffixOf` "bar"
-- False
-- >>> "" `isSuffixOf` "abc"
-- True
-- >>> "abc" `isSuffixOf` "abc"
-- True
isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf suffix xs = suffix == drop (length xs - length suffix) xs

-- | module行を見つける
--
-- >>> findModuleLine ["x = 1", "module Foo where", "y = 2"]
-- Just ("module Foo where",["y = 2"])
-- >>> findModuleLine ["x = 1"]
-- Nothing
-- >>> findModuleLine []
-- Nothing
findModuleLine :: [String] -> Maybe (String, [String])
findModuleLine [] = Nothing
findModuleLine (l : ls)
  | "module " `isPrefixOf` stripped = Just (l, ls)
  | otherwise = findModuleLine ls
  where
    stripped = dropWhile isSpace l

-- | module行をパース
--
-- >>> parseModuleLine "module Foo where"
-- ("Foo",Nothing)
-- >>> parseModuleLine "module Foo (bar, baz) where"
-- ("Foo",Just "(bar, baz)")
-- >>> parseModuleLine "  module A.B where"
-- ("A.B",Nothing)
parseModuleLine :: String -> (String, Maybe String)
parseModuleLine l =
  let stripped = dropWhile isSpace l
      afterModule = dropWhile isSpace $ drop 7 stripped -- "module " を除去
      (name, rest) = span (\c -> not (isSpace c) && c /= '(') afterModule
      exports = extractExportList rest
   in (name, exports)

-- | export listを抽出
--
-- >>> extractExportList "(foo, bar) where"
-- Just "(foo, bar)"
-- >>> extractExportList " where"
-- Nothing
-- >>> extractExportList ""
-- Nothing
-- >>> extractExportList "(a, (b, c)) where"
-- Just "(a, (b, c))"
extractExportList :: String -> Maybe String
extractExportList s =
  let trimmed = dropWhile isSpace s
   in if null trimmed || head trimmed /= '('
        then Nothing
        else Just $ takeParens trimmed

-- | 括弧の対応を取って抽出
--
-- >>> takeParens "(a, b) where"
-- "(a, b)"
-- >>> takeParens "(a, (b, c), d) rest"
-- "(a, (b, c), d)"
-- >>> takeParens "()"
-- "()"
takeParens :: String -> String
takeParens = go 0
  where
    go _ [] = []
    go n (c : cs)
      | c == '(' = c : go (n + 1) cs
      | c == ')' = if n == 1 then [c] else c : go (n - 1) cs
      | otherwise = c : go n cs

-- | import文と本体を分離（INLINEプラグマは本体に含める、複数行importに対応）
--
-- >>> extractImportsAndBody ["import Data.List", "x = 1"]
-- (["import Data.List"],["x = 1"])
-- >>> fst (extractImportsAndBody ["import Foo", "  ( a", "  , b", "  )"])
-- ["import Foo\n  ( a\n  , b\n  )"]
-- >>> extractImportsAndBody ["module M where", "", "x = 1"]
-- ([],["x = 1"])
extractImportsAndBody :: [String] -> ([String], [String])
extractImportsAndBody = go [] []
  where
    go imps body [] = (reverse imps, reverse body)
    go imps body (l : ls)
      | isImportLine l =
          let (block, rest) = collectImportBlock l ls
           in go (block : imps) body rest
      | isSkipLine l = go imps body ls
      | otherwise = go imps (l : body) ls

    isImportLine l =
      let trimmed = dropWhile isSpace l
       in "import " `isPrefixOf` trimmed

    -- スキップするのはmodule行と先頭に集約するプラグマのみ
    isSkipLine l =
      let trimmed = dropWhile isSpace l
       in null trimmed
            || "module " `isPrefixOf` trimmed
            || isTopLevelPragmaLine trimmed

    -- 先頭に集約するプラグマ行かどうか
    isTopLevelPragmaLine trimmed =
      "{-#" `isPrefixOf` trimmed
        && ( isPragmaType "LANGUAGE" trimmed
               || isPragmaType "OPTIONS_GHC" trimmed
               || isPragmaType "OPTIONS" trimmed
           )

-- | import行から、必要なら複数行に渡るimportブロックを取り出す
--
-- 例:
--   import Foo
--     ( a,
--       b
--     )
-- のように、import行に括弧が無いがすぐ次の行から ( で始まる場合も取り込む。
--
-- >>> collectImportBlock "import Data.List (foo, bar)" ["x = 1"]
-- ("import Data.List (foo, bar)",["x = 1"])
-- >>> collectImportBlock "import Foo" ["  ( a", "  , b", "  )", "x = 1"]
-- ("import Foo\n  ( a\n  , b\n  )",["x = 1"])
-- >>> collectImportBlock "import Foo (a," ["  b, c)", "x = 1"]
-- ("import Foo (a,\n  b, c)",["x = 1"])
-- >>> collectImportBlock "import Bar" ["x = 1"]
-- ("import Bar",["x = 1"])
collectImportBlock :: String -> [String] -> (String, [String])
collectImportBlock firstLine restLines
  | bal > 0 =
      let (cont, after) = takeUntilBalanced bal restLines
       in (intercalate "\n" (firstLine : cont), after)
  | bal == 0 = case restLines of
      (next : ls')
        | startsWithOpenParen next ->
            let nbal = parenBalance next
                (cont, after)
                  | nbal > 0 = takeUntilBalanced nbal ls'
                  | otherwise = ([], ls')
             in (intercalate "\n" (firstLine : next : cont), after)
      _ -> (firstLine, restLines)
  | otherwise = (firstLine, restLines)
  where
    bal = parenBalance firstLine

-- | 行頭の空白を除いた最初の文字が '(' かどうか
--
-- >>> startsWithOpenParen "  ( a, b )"
-- True
-- >>> startsWithOpenParen "abc"
-- False
-- >>> startsWithOpenParen ""
-- False
-- >>> startsWithOpenParen "("
-- True
startsWithOpenParen :: String -> Bool
startsWithOpenParen l = case dropWhile isSpace l of
  ('(' : _) -> True
  _ -> False

-- | 括弧の対応を取って、バランスが0になる行までを取り出す
--
-- >>> takeUntilBalanced 1 ["a, b )"]
-- (["a, b )"],[])
-- >>> takeUntilBalanced 1 ["a, b", "  c)", "rest"]
-- (["a, b","  c)"],["rest"])
-- >>> takeUntilBalanced 1 []
-- ([],[])
-- >>> takeUntilBalanced 2 ["a", "b)", "c)", "rest"]
-- (["a","b)","c)"],["rest"])
takeUntilBalanced :: Int -> [String] -> ([String], [String])
takeUntilBalanced _ [] = ([], [])
takeUntilBalanced bal (l : ls)
  | newBal <= 0 = ([l], ls)
  | otherwise =
      let (cont, after) = takeUntilBalanced newBal ls
       in (l : cont, after)
  where
    newBal = bal + parenBalance l

-- | 文字列中の括弧収支を計算 (簡易: 文字列リテラル/コメントは考慮しない)
--
-- >>> parenBalance "(abc)"
-- 0
-- >>> parenBalance "( a, b"
-- 1
-- >>> parenBalance "a, b )"
-- -1
-- >>> parenBalance "((x))"
-- 0
-- >>> parenBalance ""
-- 0
parenBalance :: String -> Int
parenBalance = sum . map balanceChar
  where
    balanceChar '(' = 1
    balanceChar ')' = -1
    balanceChar _ = 0

-- | import文をパース
--
-- >>> impName (parseImport "import Data.List")
-- "Data.List"
-- >>> impQualified (parseImport "import qualified Data.Map as M")
-- True
-- >>> impAs (parseImport "import qualified Data.Map as M")
-- Just "M"
-- >>> impHiding (parseImport "import Data.List hiding (foo)")
-- True
-- >>> impList (parseImport "import Data.List (foo, bar)")
-- Just "(foo, bar)"
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
--
-- >>> extractModuleName ["import", "Data.List"]
-- "Data.List"
-- >>> extractModuleName ["import", "qualified", "Data.Map"]
-- "Data.Map"
-- >>> extractModuleName ["import", "Foo(bar)"]
-- "Foo"
-- >>> extractModuleName []
-- ""
extractModuleName :: [String] -> String
extractModuleName ws =
  case dropWhile (\w -> w `elem` ["import", "qualified"]) ws of
    (name : _) -> takeWhile (\c -> c /= '(' && not (isSpace c)) name
    [] -> ""

-- | as句を抽出
--
-- >>> extractAs ["import", "qualified", "Data.Map", "as", "M"]
-- Just "M"
-- >>> extractAs ["import", "Data.List"]
-- Nothing
-- >>> extractAs []
-- Nothing
extractAs :: [String] -> Maybe String
extractAs ws =
  case dropWhile (/= "as") ws of
    (_ : name : _) -> Just name
    _ -> Nothing

-- | import listを抽出
--
-- >>> extractImportList "import Data.List (foo, bar)"
-- Just "(foo, bar)"
-- >>> extractImportList "import Data.List"
-- Nothing
-- >>> extractImportList "import qualified Data.Map as M"
-- Nothing
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
--
-- >>> map modName (topSort M.empty)
-- []
-- >>> map modName (topSort (M.fromList [("F", parseModule "F.hs" "module F where")]))
-- ["F"]
topSort :: M.Map String ModuleInfo -> [ModuleInfo]
topSort mods = reverse $ snd $ go S.empty [] (M.keys mods)
  where
    go visited acc [] = (visited, acc)
    go visited acc (name : rest)
      | name `S.member` visited = go visited acc rest
      | otherwise =
          case M.lookup name mods of
            Nothing -> go visited acc rest
            Just info ->
              let deps = filter (`M.member` mods) (map impName $ modImports info)
                  visited' = S.insert name visited
                  (visited'', acc') = go visited' acc deps
               in go visited'' (info : acc') rest

-- | バンドル出力を生成
--
-- >>> let m = parseModule "M.hs" "module Main where\nmain = return ()"
-- >>> lines (generateBundle m [] M.empty) !! 1
-- "module Main where"
-- >>> let m = parseModule "M.hs" "module Main where\nmain = return ()"
-- >>> last (lines (generateBundle m [] M.empty))
-- "main = return ()"
generateBundle :: ModuleInfo -> [ModuleInfo] -> M.Map String ModuleInfo -> String
generateBundle mainMod localMods localModMap =
  unlines $
    -- 1. 全プラグマを集約（LANGUAGEとOPTIONS_GHCのみ）
    nub (concatMap modPragmas (mainMod : localMods))
      ++ [""]
      ++
      -- 2. module Main where
      ["module Main where", ""]
      ++
      -- 3. 外部ライブラリのimportのみ残す（ローカルへの qualified import は捨てる）
      externalImports
      ++ [""]
      ++
      -- 4. ローカルモジュールの本体を展開（INLINEプラグマ含む、ローカル qualified prefix を除去）
      concatMap (expandModule localModMap) localMods
      ++
      -- 5. Main本体（ローカル qualified prefix を除去）
      ["", "-- Main"]
      ++ map (stripQualifiers (qualifiedLocalPrefixes localModMap mainMod)) (modBody mainMod)
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

-- | qualified なローカルimportの prefix（asがあればそれ、なければモジュール名）を抽出
--
-- >>> let m = parseModule "X.hs" "module X where\nimport qualified Foo as F\nimport qualified Bar\nimport qualified Data.Map as DM"
-- >>> qualifiedLocalPrefixes (M.fromList [("Foo", undefined), ("Bar", undefined)]) m
-- ["F","Bar"]
-- >>> qualifiedLocalPrefixes M.empty (parseModule "X.hs" "module X where\nimport qualified Foo as F")
-- []
qualifiedLocalPrefixes :: M.Map String ModuleInfo -> ModuleInfo -> [String]
qualifiedLocalPrefixes localModMap info =
  [ fromMaybe (impName imp) (impAs imp)
    | imp <- modImports info,
      impQualified imp,
      impName imp `M.member` localModMap
  ]

-- | 指定したqualifier prefixをコードから除去する
--
-- 識別子境界を考慮して "Pfx." だけを取り除く。文字列リテラルやコメントは考慮しない簡易版。
--
-- >>> stripQualifier "M" "M.foo + M.bar"
-- "foo + bar"
-- >>> stripQualifier "M" "xM.foo"
-- "xM.foo"
-- >>> stripQualifier "M" "f (M.x) M.y"
-- "f (x) y"
-- >>> stripQualifier "M" ""
-- ""
-- >>> stripQualifier "Foo.Bar" "Foo.Bar.x + Foo.Bar.y"
-- "x + y"
stripQualifier :: String -> String -> String
stripQualifier prefix = go True
  where
    p = prefix ++ "."
    np = length p
    isIdent c = isAlphaNum c || c == '_' || c == '\''
    go _ [] = []
    go boundary s@(c : cs)
      | boundary && take np s == p = go True (drop np s)
      | otherwise = c : go (not (isIdent c)) cs

-- | 複数の prefix をまとめて除去
--
-- >>> stripQualifiers ["M", "N"] "M.foo + N.bar + x"
-- "foo + bar + x"
-- >>> stripQualifiers [] "M.foo"
-- "M.foo"
stripQualifiers :: [String] -> String -> String
stripQualifiers prefixes = foldr (.) id (map stripQualifier prefixes)

-- | モジュールを展開（qualified なローカルimportの prefix を除去）
--
-- >>> let mods = M.fromList [("Foo", undefined)]
-- >>> let m = parseModule "Bar.hs" "module Bar where\nimport qualified Foo as F\nbar = F.foo + 1"
-- >>> expandModule mods m
-- ["","-- Bar","bar = foo + 1"]
-- >>> expandModule M.empty (parseModule "Bar.hs" "module Bar where")
-- ["","-- Bar"]
expandModule :: M.Map String ModuleInfo -> ModuleInfo -> [String]
expandModule localModMap info =
  ["", "-- " ++ modName info] ++ map strip (modBody info)
  where
    strip = stripQualifiers (qualifiedLocalPrefixes localModMap info)
