{ mkDerivation, base, extra, font-awesome-type, hpack, stdenv, time
}:
mkDerivation {
  pname = "acpiPower";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base extra font-awesome-type time ];
  prePatch = "hpack";
  homepage = "https://github.com/ssbothwell/acpiPower#readme";
  license = stdenv.lib.licenses.bsd3;
}
