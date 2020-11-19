{ mkDerivation, base, extra, font-awesome-type, stdenv, time }:
mkDerivation {
  pname = "acpiPower";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base extra font-awesome-type time ];
  homepage = "https://github.com/ssbothwell/acpiPower#readme";
  license = stdenv.lib.licenses.bsd3;
}
