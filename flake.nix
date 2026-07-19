{
  description = "AtCoder Haskell environment (GHC 9.8.4 + cabal)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system:
        f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs:
        let
          # Makefile の `oj test` 用。
          # packages に python パッケージとして入れると setup hook が依存クロージャを
          # PYTHONPATH に export し、シェル内で起動する他の python ツール
          # （acc 経由の uv 版 oj など）を壊すため、自己完結の env にして PATH にだけ載せる。
          # lxml は AtCoder の HTML パースに必須（無いと oj download が Sample not found になる）。
          ojEnv = pkgs.python3.withPackages (ps: [
            ps.online-judge-tools
            ps.lxml
          ]);
        in
        {
          default = pkgs.mkShell {
            packages = [
              # cabal.project.local の with-compiler: ghc-9.8.4 に合わせる
              pkgs.haskell.compiler.ghc984
              pkgs.cabal-install
              pkgs.haskell.packages.ghc984.haskell-language-server
            ];
            shellHook = ''
              export PATH=${ojEnv}/bin:$PATH
            '';
          };
        });
    };
}
