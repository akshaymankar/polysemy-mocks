{ mkDerivation, base, hspec, hspec-discover, lib, polysemy
, template-haskell
}:
mkDerivation {
  pname = "polysemy-mocks";
  version = "0.3.1.0";
  src = ./.;
  libraryHaskellDepends = [ base polysemy template-haskell ];
  testHaskellDepends = [ base hspec polysemy ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/akshaymankar/polysemy-mocks#readme";
  description = "Mocking framework for polysemy effects";
  license = lib.licenses.bsd3;
}
