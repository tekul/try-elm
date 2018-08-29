{ mkDerivation, aeson, array, base, blaze-html, blaze-markup
, bytestring, containers, directory, exceptions, filepath, mtl
, process, servant-blaze, servant-server, stdenv, text, time, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "try-elm";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson array base blaze-html blaze-markup bytestring containers
    directory exceptions filepath mtl process servant-blaze
    servant-server text time wai wai-extra warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
