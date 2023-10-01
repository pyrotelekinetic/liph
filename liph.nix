{ mkDerivation, base, lib, mtl }:
mkDerivation {
  pname = "liph";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base mtl ];
  description = "A lisp dialect written in haskell";
  license = lib.licenses.agpl3Plus;
  mainProgram = "liph";
}
