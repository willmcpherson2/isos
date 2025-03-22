{
  description = "Haskell LLVM development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      basePkgs = [
        pkgs.cabal-install
        pkgs.haskell.compiler.ghc910
        pkgs.haskellPackages.cabal-fmt
        pkgs.haskellPackages.ormolu
        pkgs.clang-tools
        pkgs.llvmPackages_19.llvm
      ];
      baseShellHook = ''
        export LD_LIBRARY_PATH=${pkgs.llvmPackages_19.llvm.lib}/lib:$LD_LIBRARY_PATH
      '';
    in
    {
      devShells.${system} = {
        default = pkgs.mkShell {
          buildInputs = basePkgs ++ [
            (pkgs.haskell-language-server.override { supportedGhcVersions = [ "910" ]; })
            pkgs.haskellPackages.hoogle
          ];
          shellHook = baseShellHook;
        };
        ci = pkgs.mkShell {
          buildInputs = basePkgs;
          shellHook = baseShellHook;
        };
      };
    };
}
