#!/usr/bin/env bash
# src/ と tools/ 配下の全モジュールの doctest を実行する。
#
# doctest は複数モジュールを 1 つの GHCi セッションに読み込むため、名前を共有する
# モジュール同士（例: BSearchArray/BSearchVector）が干渉して順序依存で誤判定する。
# それを避けるため、モジュールごとに別プロセスで隔離実行する。
# モジュール一覧は src/ と tools/ から自動で集めるので cabal の exposed-modules と
# 二重管理にならない。tools/ を含めるのはバンドラ（Bundle.hs）の doctest が
# 放置されて腐るのを防ぐため。
#
# 使い方:
#   scripts/doctest.sh            # 全モジュール
#   scripts/doctest.sh src/Math.hs src/ModInt.hs  # 指定モジュールのみ
set -uo pipefail
cd "$(dirname "$0")/.."

if [ "$#" -gt 0 ]; then
  files=("$@")
else
  # シェルに依存しないよう find の結果を読み込む
  files=()
  while IFS= read -r f; do files+=("$f"); done < <(find src tools -name '*.hs' | sort)
fi

fail=0
for f in "${files[@]}"; do
  echo "== doctest $f =="
  # include path はファイル自身のディレクトリ（src/ なら -isrc、tools/ なら -itools）
  if ! cabal exec -v0 -- doctest -i"$(dirname "$f")" "$f"; then
    fail=1
  fi
done

if [ "$fail" -ne 0 ]; then
  echo "doctest: FAILED" >&2
  exit 1
fi
echo "doctest: all passed"
