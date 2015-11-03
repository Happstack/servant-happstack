{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, case-insensitive, either, happstack-server, http-api-data, http-types, network, servant, stdenv, bytestring, mmorph, mtl, transformers}:
      mkDerivation {
        pname = "servant-happstack";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base case-insensitive happstack-server http-api-data http-types either network servant bytestring mmorph mtl transformers ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
