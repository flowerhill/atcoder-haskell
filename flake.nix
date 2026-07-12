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
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            # cabal.project.local の with-compiler: ghc-9.8.4 に合わせる
            pkgs.haskell.compiler.ghc984
            pkgs.cabal-install
            pkgs.haskell.packages.ghc984.haskell-language-server
            # Makefile の `oj test` 用
            pkgs.online-judge-tools
          ];
        };
      });
    };
}
