{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, free, hpack, random, stdenv }:
      mkDerivation {
        pname = "occurrence-control";
        version = "0.9.0";
        src = ./.;
        libraryHaskellDepends = [ base free random ];
        libraryToolDepends = [ hpack ];
        preConfigure = "hpack";
        description = "Control of event occurrence in random generators";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
