# Haskell Contest Project

AtCoder競技プログラミング用のHaskellプロジェクトテンプレートです。

## 🚀 クイックスタート

```bash
# 依存関係のインストール
make deps

# テスト実行
make test

# 提出
make submit
```

## 📁 プロジェクト構成

```
.
├── my-contest.cabal     # Cabalプロジェクト設定
├── cabal.project        # Cabal設定（最適化等）
├── Makefile            # ビルド・テスト・提出の自動化
├── app/
│   ├── Main.hs         # メインプログラム
│   ├── Lib.hs          # 共通ライブラリ
│   └── Types.hs        # 型定義
└── test/               # ojで取得したテストケース
    ├── sample-1.in
    ├── sample-1.out
    └── ...
```

## 🛠️ 利用可能なコマンド

### ビルド関連

| コマンド     | 説明                               |
| ------------ | ---------------------------------- |
| `make build` | プロジェクトのビルド（最適化有効） |
| `make deps`  | 依存関係のインストール             |
| `make clean` | ビルド成果物のクリーンアップ       |

### テスト関連

| コマンド    | 説明                         |
| ----------- | ---------------------------- |
| `make test` | サンプルケースでのテスト実行 |

### 提出関連

| コマンド         | 説明                        |
| ---------------- | --------------------------- |
| `make submit`    | テスト後にaccで提出（推奨） |
| `make submit-oj` | ojコマンドで提出            |

## 📝 開発フロー

### 1. 問題の取得
ojまたはaccで問題を取得してください：

```bash
# ojを使用
oj dl https://atcoder.jp/contests/abc300/tasks/abc300_a

# accを使用
acc new abc300
cd abc300/a
```

### 2. コーディング
`app/Main.hs`を編集してソリューションを実装：

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Lib
import Types
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    input <- BS.getLine
    let result = solve input
    print result
```

### 3. テスト
サンプルケースでテスト：

```bash
make test
```

### 4. 提出
テストが通ったら提出：

```bash
make submit
```

## ⚙️ 設定

### Cabal設定
`my-contest.cabal`でライブラリの依存関係を管理：

```cabal
build-depends:    base ^>=4.17
                , vector ^>=0.13
                , containers ^>=0.6.7
                , bytestring ^>=0.11.4.0
                , text ^>=2.0
```

### 最適化設定
`cabal.project`で最適化オプションを設定：

```
optimization: 2
executable-stripping: True
```

## 🔧 トラブルシューティング

### テストが失敗する場合
```bash
# 手動でテスト
cabal run main < test/sample-1.in
```

### 提出が失敗する場合
```bash
# ojで提出
make submit-oj
```

### ビルドエラーが発生する場合
```bash
# 依存関係の再インストール
make clean
make deps
make build
```

## 📖 参考資料

- [AtCoder](https://atcoder.jp/)
- [online-judge-tools](https://github.com/online-judge-tools/oj)
- [atcoder-cli](https://github.com/Tatamo/atcoder-cli)
- [Haskell公式](https://www.haskell.org/)
