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
    in
    with pkgs;
    {
      devShells.${system}.default = mkShell {
        buildInputs = [
          cabal-install
          haskell.compiler.ghc910
          (haskell-language-server.override { supportedGhcVersions = [ "910" ]; })
          haskellPackages.cabal-fmt
          haskellPackages.hoogle
          ormolu

          clang-tools
          llvmPackages_19.llvm
        ];
        shellHook = ''
          export LD_LIBRARY_PATH=${llvmPackages_19.llvm.lib}/lib:$LD_LIBRARY_PATH
        '';
      };
    };
}
