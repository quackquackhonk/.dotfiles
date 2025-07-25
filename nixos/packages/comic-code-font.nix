# make a  derivation for berkeley-mono font installation
{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "comic-code-font";
  version = "1.0";

  src = ../../fonts/comic-code;

  installPhase = ''
    runHook preInstall

    install -Dm644 $src/*.otf -t $out/share/fonts/opentype

    runHook postInstall
  '';
}
