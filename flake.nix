{
  description = "LLVM C++ development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          cabal-install
          haskell.compiler.ghc910
          (haskell-language-server.override { supportedGhcVersions = [ "910" ]; })
          haskellPackages.cabal-fmt
          haskellPackages.hoogle
          ormolu

          clang-tools
          llvmPackages_19.clang
          llvmPackages_19.llvm
        ];
      };
    };
}
