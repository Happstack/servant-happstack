{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, either, happstack-server, servant, stdenv, bytestring, mmorph, mtl, transformers, servant-todo-common }:
      mkDerivation {
        pname = "servant-happstack";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base happstack-server either servant bytestring mmorph mtl transformers servant-todo-common ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
