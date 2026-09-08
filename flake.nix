{
  description = "Haskell Ray Tracer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [
      "x86_64-linux"
      "aarch64-linux"
    ];

    perSystem = { pkgs, ... }: let
      hsPkgs = pkgs.haskellPackages;
    in {
      devShells.default = hsPkgs.shellFor {
        packages = p: [];

        nativeBuildInputs = [
          hsPkgs.cabal-install
          hsPkgs.haskell-language-server
        ];

        strictDeps = true;

        shellHook = ''
          echo "To build: \`cabal build .\`"
          echo "To run: \`cabal run . -- [PATH TO FILE]\`"
        '';
      };
    };
  };
}
