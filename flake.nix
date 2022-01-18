{
  description = "nix flake for hackage-overlay-repo-tool";
  inputs.nixpkgs.url = "github:angerman/nixpkgs/patch-1"; # based on 21.11, still need this, until everything is merged into 21.11.
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.haskellNix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, haskellNix, nixpkgs, flake-utils }:
    let systems = [ "x86_64-linux" ]; in
    flake-utils.lib.eachSystem systems (system:
      let pkgs = haskellNix.legacyPackages.${system}; in
      let drv = pkgs': pkgs'.haskell-nix.project {
        compiler-nix-name = "ghc8107";
        index-state = "2022-01-15T00:00:00Z";
        # We need this, to specify we want the cabal project.
        # If the stack.yaml was dropped, this would not be necessary.
        projectFileName = "cabal.project";
        src = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "hackage-overlay-repo-tool";
          src = ./.;
        };
        sha256map = {
            "https://github.com/haskell/hackage-security.git"."f45ae75cf58e9c614451106f445c0cd5d4a2e9b9" = "02zi6kaczmb21lxvc01wsqq38inq0l3yn88c4bgahlf4ikjb1yfn";
        };
      }; in
      rec {
          packages = ({
              "x86_64-linux" = rec {
                  hackage-overlay-repo-tool = (drv pkgs.pkgsCross.musl64).hackage-overlay-repo-tool.components.exes.hackage-overlay-repo-tool;
                  hackage-repo-tool = (drv pkgs.pkgsCross.musl64).hackage-repo-tool.components.exes.hackage-repo-tool;
                  tarball = pkgs.stdenv.mkDerivation {
                      name = "hackage-overlay-tools";
                      phases = [ "installPhase" ];
                      installPhase = ''
                        mkdir -p $out/_pkg
                        cp ${hackage-overlay-repo-tool}/bin/* $out/_pkg
                        cp ${hackage-repo-tool}/bin/* $out/_pkg
                        chmod +w $out/_pkg/*
                        strip $out/_pkg/*

                        (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/pkg.zip *)
                        rm -fR $out/_pkg
                        mkdir -p $out/nix-support
                        echo "file binary-dist \"$(echo $out/*.zip)\"" \
                           > $out/nix-support/hydra-build-products
                      '';
                  };
              };
          }.${system} or {});
        # build all packages in hydra.
        hydraJobs = packages;
      }
    );
}