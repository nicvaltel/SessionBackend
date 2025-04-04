{ nixpkgs ? import <nixpkgs> {  } }:
let
  # You can get a newer ref by looking under "nixpkgs-unstable" in https://status.nixos.org/
  nixpkgsRev = "ea5234e7073d5f44728c499192544a84244bf35a";
  nixpkgsSha = "sha256:1iqfglh1fdgqxm7n4763k1cipna68sa0cb3azm2gdzhr374avcvk";
  compiler = pkgs.haskellPackages;
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = nixpkgsSha;
  }) {} ;


in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs =  [
      compiler.stack
      compiler.cabal-install
      compiler.ghcid
      compiler.haskell-language-server
      compiler.ghcide
      pkgs.ormolu
      pkgs.hpack
      compiler.ghc

      pkgs.haskellPackages.record-dot-preprocessor
      pkgs.zlib
      pkgs.pcre # for pcre-heavy dependency in haskell
      pkgs.postgresql
      pkgs.libpqxx
      pkgs.haskellPackages.postgresql-libpq

      pkgs.nodePackages.live-server
      pkgs.websocat # Websocat is a command-line utility that can help you test WebSocket connections

      pkgs.python310Packages.websockets
      pkgs.python310Packages.requests
    ];
}


# To run HLS:
# stack new MyProgram
# rm Setup.hs 
# rm stack.yaml 
# rm MyProgram.cabal
# rm -rf .stack-work/
# hpack
# gen-hie > hie.yaml
# cabal build