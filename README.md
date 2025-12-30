# AtCoder Haskell Template

AtCoder競技プログラミング用のHaskellプロジェクトテンプレートです。

## 📋 必要なツール

- `cabal` (Haskell build tool)
- `oj` (online-judge-tools)
- `acc` (atcoder-cli)

## 🚀 セットアップ

```bash
# テンプレートをaccのconfig-dirに配置
cp -r atcoder-haskell `acc config-dir`/
# default-templateに設定 (お好みで)
acc config default-template atcoder-haskell
```

## 📁 プロジェクト構成

```
.
├── atcoder-haskell.cabal   # Cabalプロジェクト設定
├── Makefile               # ビルド・テスト・提出の自動化
├── app/
│   └── Main.hs           # メインプログラム
├── src/                  # ライブラリモジュール
│   ├── Lib.hs           # 基本ライブラリ
│   ├── Input.hs         # 入力処理
│   ├── Util.hs          # ユーティリティ
│   ├── Array.hs         # 配列操作
│   ├── Graph.hs         # グラフアルゴリズム
│   ├── Dijkstra.hs     # ダイクストラ法
│   └── ...              # その他のアルゴリズム
├── spec/                 # テストコード
├── tools/
│   └── Bundle.hs        # ファイル結合ツール
└── test/                # ojで取得したテストケース
    ├── sample-1.in
    ├── sample-1.out
    └── ...
```

## 🛠️ 利用可能なコマンド

### ビルド関連

| コマンド         | 説明                               |
| --------------- | ---------------------------------- |
| `make build`    | プロジェクトのビルド（最適化有効） |
| `make build-debug` | デバッグビルド                   |
| `make deps`     | 依存関係のインストール             |
| `make clean`    | ビルド成果物のクリーンアップ       |

### テスト関連

| コマンド         | 説明                              |
| --------------- | --------------------------------- |
| `make test`     | サンプルケースでのテスト実行       |
| `make test-debug` | デバッグモードでテスト実行        |
| `make test-case CASE=N` | 特定のテストケースを実行 |
| `make test-bundle` | バンドルファイルでテスト実行     |
| `make unit-test` | ユニットテスト実行               |

### 提出関連

| コマンド         | 説明                        |
| --------------- | --------------------------- |
| `make submit`   | テスト後にaccで提出（推奨） |
| `make submit-oj` | ojコマンドで提出           |
| `make bundle`   | ファイルを結合して提出用ファイル作成 |

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
import Input
import Util
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    input <- BS.getLine
    let result = solve input
    print result

solve :: BS.ByteString -> Int
solve input = -- ソリューションを実装
```

### 3. テスト

サンプルケースでテスト：

```bash
# 通常のテスト
make test

# デバッグモードでテスト
make test-debug

# 特定のケースのみテスト
make test-case CASE=1
```

### 4. 提出

テストが通ったら提出：

```bash
# バンドルファイルを作成して提出
make submit

# ojで提出
make submit-oj
```

## 📚 ライブラリモジュール

プロジェクトには以下の便利なライブラリが含まれています：

- **Input.hs**: 入力処理のヘルパー関数
- **Util.hs**: 汎用ユーティリティ関数
- **Array.hs**: 配列操作関数
- **Graph.hs**: グラフアルゴリズム
- **Dijkstra.hs**: ダイクストラ法の実装
- **UnionFindIOUArray.hs**: Union-Find構造
- **BSearchArray.hs / BSearchVector.hs**: 二分探索
- **Encode.hs**: エンコード関連
- **IntMultiSet.hs**: 整数のマルチセット
- **MyList.hs**: リスト操作拡張

## ⚙️ 設定

### 依存ライブラリ

主な依存ライブラリ：
- `vector` - 高性能配列
- `containers` - Map, Set等のデータ構造
- `bytestring` - 高速文字列処理
- `array` - 配列操作
- `unordered-containers` - ハッシュマップ
- `multiset` - マルチセット
- `heaps` - ヒープ構造

### 最適化設定

Cabalプロジェクトで最適化オプションが設定されています：
- `-O2`: 最適化レベル2
- `-funbox-strict-fields`: 厳格フィールドの最適化
- `-fexcess-precision`: 精度向上

## 🔧 トラブルシューティング

### テストが失敗する場合
```bash
# 手動でテスト
cabal run main < test/sample-1.in

# デバッグモードで確認
make test-debug
```

### 提出が失敗する場合
```bash
# バンドルファイルの構文チェック
make verify-bundle

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