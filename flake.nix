{

description = "A lisp dialect written in haskell";

inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

outputs = { self, nixpkgs }: let
  pkgs = nixpkgs.legacyPackages.x86_64-linux;
  lib = pkgs.lib;
  haskellPackages = pkgs.haskell.packages.ghc94;
  liph = haskellPackages.callPackage ./liph.nix { };
in {
  packages.x86_64-linux.default = liph;

  devShells.x86_64-linux.default = haskellPackages.shellFor {
    packages = lib.const [ liph ];
    nativeBuildInputs = with haskellPackages; [
      ghcid
      hlint
      cabal2nix
      cabal-install
    ];
  };
};

}
