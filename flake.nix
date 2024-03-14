{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=c7eb65213bd7d95eafb8c5e2e181f04da103d054";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, flake-compat, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [];
      perSystem = { self', system, ... }:
      let pkgs = import nixpkgs { inherit system; };
          t = pkgs.lib.trivial;
          hl = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."ghc94";
          project = runTests: executable-name: devTools: # [1]
            let addBuildTools = (t.flip hl.addBuildTools) devTools;
                addBuildDepends = (t.flip hl.addBuildDepends)
                  [  ];
                doRunTests =
                  if runTests then hl.doCheck else hl.dontCheck;
            in compiler.developPackage {
              root = pkgs.lib.sourceFilesBySuffices ./.
                       [ ".cabal"
                         ".hs"
                         ".tel"
                         "cases"
                         "LICENSE"
                       ];
              name = executable-name;
              source-overrides = {
                vty-crossplatform = dep/vty-crossplatform;
                telomare = dep/stand-in-language;
              };
              returnShellEnv = !(devTools == [ ]); # [2]

              modifier = (t.flip t.pipe) [
                addBuildDepends
                addBuildTools
                doRunTests
                # hl.dontHaddock
              ];
            };

      in {
        packages.reflex-vty = project false "reflex-vty" [ ]; # [3]
        packages.default = self.packages.${system}.reflex-vty;

        devShells.default = project true "reflex-vty-with-tools" (with compiler; [ # [4]
          cabal-install
          haskell-language-server
          hlint
          ghcid
          stylish-haskell
        ]);

        checks = {
          build-and-tests = project true "telomare-with-tests" [ ];
        };
      };
    };
}
