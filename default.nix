{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822", doBenchmark ? false, doCheck ? false }:

with pkgs.haskell.lib;

let

  f = { mkDerivation, base, containers, optparse-applicative
      , shelly, stdenv, system-filepath, text
      }:
      mkDerivation {
        pname = "tool";
        version = "0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers optparse-applicative (dontCheck shelly) system-filepath text
        ];
        description = "Tool for generating head.hackage-style repos";
        license = stdenv.lib.licenses.gpl3;
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        configureFlags = [
          "--ghc-option=-threaded"
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
          "--ghc-option=-optl=-L${pkgs.gmp5.static}/lib"
          "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
        ];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = p: let check = if doCheck then pkgs.haskell.lib.doCheck else pkgs.haskell.lib.dontCheck;
                   bench = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
               in check (bench p);

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
