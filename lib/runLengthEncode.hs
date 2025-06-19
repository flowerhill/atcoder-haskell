import Data.List (group)

-- エンコード: 文字列を圧縮する
encode :: String -> [(Int, Char)]
encode = map (\xs -> (length xs, head xs)) . group

-- デコード: 圧縮された形式から元の文字列に戻す
decode :: [(Int, Char)] -> String
decode = concatMap (uncurry replicate)

-- エンコードの結果を文字列として表現
encodeToString :: String -> String
encodeToString = concatMap (\(n, c) -> show n ++ [c]) . encode

-- 文字列からデコード
decodeFromString :: String -> String
decodeFromString "" = ""
decodeFromString s = decode $ parseEncoded s
  where
    parseEncoded :: String -> [(Int, Char)]
    parseEncoded [] = []
    parseEncoded str =
      let (nums, rest) = span isDigit str
       in case rest of
            (c : rs) -> (read nums, c) : parseEncoded rs
            [] -> []

    isDigit c = c >= '0' && c <= '9'

-- テスト用の補助関数
test :: String -> IO ()
test input = do
  putStrLn $ "Input: " ++ input
  let encoded = encodeToString input
  putStrLn $ "Encoded: " ++ encoded
  let decoded = decodeFromString encoded
  putStrLn $ "Decoded: " ++ decoded
  putStrLn $ "Successful: " ++ show (input == decoded)
  putStrLn ""

-- メイン関数（テスト実行用）
main :: IO ()
main = do
  test "AABBBCCCC"
  test "WWWWWWWWWWWWBWWWWWWWWWWWW"
  test "HelloWorld"