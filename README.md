# AtCoder Haskell Template

AtCoder競技プログラミング用のHaskellプロジェクトテンプレートです。

## 📋 必要なツール

- `cabal` (Haskell build tool)
- `oj` (online-judge-tools)
- `acc` (atcoder-cli)
- `ormolu` (Haskell formatter) — `make format` / `make bundle` で提出ファイルの整形に使用。`brew install ormolu` 等で導入

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
│   ├── Lib.hs           # solve のひな形
│   ├── Input.hs         # 入力処理
│   ├── Math.hs          # 数論・整数演算
│   ├── ModInt.hs        # mod 10^9+7 演算
│   ├── Graph.hs         # グラフアルゴリズム
│   └── ...              # その他のアルゴリズム
├── spec/                 # テストコード
├── scripts/
│   └── doctest.sh       # モジュール毎の doctest 実行
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
| `make test-bundle` | サンプルテスト + 提出ファイル作成 |
| `make unit-test` | ユニットテスト実行               |
| `make doctest`  | 各モジュールの doctest を実行       |
| `make test-all` | サンプルテスト + doctest           |

### 提出ファイル作成

| コマンド         | 説明                        |
| --------------- | --------------------------- |
| `make gen-bundle` | ファイルを結合するだけ（整形・検査なし） |
| `make format`   | 提出ファイルを ormolu で整形（要 ormolu） |
| `make verify-bundle` | 提出ファイルを型検査（`ghc -fno-code`） |
| `make bundle`   | 結合 → 整形 → 型検査 をまとめて実行 |

### 提出関連

| コマンド         | 説明                        |
| --------------- | --------------------------- |
| `make submit-file` | テスト + 提出ファイル作成まで（手動提出用） |
| `make submit`   | テスト後にaccで提出         |
| `make submit-oj` | ojコマンドで提出           |

> **Note**: `acc submit` は内部で `oj`（online-judge-tools）を使うため、`oj` の提出が壊れていると `make submit` / `make submit-oj` の両方が失敗します。その場合は `make submit-file` で `dist/Submit.hs` を用意し、AtCoder の Web から手動で提出してください。

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
module Main where

import Input (getInts)

main :: IO ()
main = do
    xs <- getInts
    print (solve xs)

solve :: [Int] -> Int
solve = sum -- ソリューションを実装
```

必要なモジュールだけを直接 import する（例: `import Math (powMod)`）。
提出時はバンドラが import したモジュールとその依存だけを 1 ファイルにまとめる。

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

- **Lib.hs**: `solve` のひな形
- **Input.hs**: 入力処理のヘルパー関数
- **Math.hs**: 数論・整数演算（素数判定・篩・nCr・拡張ユークリッドなど）
- **ModInt.hs**: mod 10^9+7 のモジュラ演算と `IntMod` 型
- **Geometry.hs**: 平面幾何（距離・座標変換・回転）
- **MyString.hs**: 文字列操作（回文判定・部分文字列）
- **Debug.hs**: DEBUG 環境変数連動のトレース
- **Graph.hs**: グラフアルゴリズム（BFS/DFS/最短路/Dijkstra/SCC/Warshall-Floyd/木DP）
- **BSearchArray.hs / BSearchVector.hs**: 二分探索
- **Encode.hs**: ランレングス圧縮
- **IntMultiSet.hs**: 整数のマルチセット
- **UnionFind.hs / WUnionFind.hs**: Union-Find / 重み付き Union-Find
- **MyArray.hs / MyMArray.hs / MyMVector.hs**: 配列・可変配列・可変ベクタ操作
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

Cabalプロジェクトで以下の ghc-options を設定しています：
- `-O2`: 最適化レベル2
- `-Wall`: 警告を有効化

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