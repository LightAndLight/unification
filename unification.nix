{ mkDerivation, base, bound, deriving-compat, equivalence, lens
, mtl, stdenv
}:
mkDerivation {
  pname = "unification";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bound deriving-compat equivalence lens mtl
  ];
  license = stdenv.lib.licenses.bsd3;
}
