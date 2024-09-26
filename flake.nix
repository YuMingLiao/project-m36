{
  #inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

  outputs = { self, nixpkgs }:
    with nixpkgs.legacyPackages.x86_64-linux;
    let
      config = {
        allowBroken = true;
        allowAliases = false;
      };
      pkgs =
        nixpkgs.legacyPackages.x86_64-linux.extend (f: p: { inherit config; });
      repo = builtins.path {
        path = ./.;
        name = "project-m36";
      };
      overlay = with pkgs.haskell.lib;
        with pkgs.lib.fileset;
        final: prev: {
          # `nix flake show` and `nix flake check` won't work because of
          # IFD (https://nixos.wiki/wiki/Import_From_Derivation).
          # Might be possible to use in the near future, check this out:
          # https://github.com/cdepillabout/cabal2nixWithoutIFD

          mkDerivation = drv:
            if drv.pname == "hscolour" then
              drv
            else
              prev.mkDerivation (drv // {
                doCheck = false;
                doHaddock = false;
                enableLibraryProfiling = false;
                enableExecutableProfiling = false;
              });
          project-m36-full =
            pkgs.lib.pipe (final.callCabal2nix "project-m36" repo { }) [
              dontCheck
              dontHaddock
              disableLibraryProfiling
              disableExecutableProfiling
            ];
          project-m36 = compose.setBuildTargets [
            "lib:project-m36"
            "exe:project-m36-server"
            "exe:tutd"
          ] final.project-m36-full;
          #barbies-th = doJailbreak prev.barbies-th;
          curryer-rpc = final.callHackageDirect {
            pkg = "curryer-rpc";
            ver = "0.3.5";
            sha256 = "sha256-7mEJOBKzA2rTnLxZme8E6zFv0VkiXBo5L/jUJSNPaNE=";
          } { };
          #streamly = final.callHackageDirect {
          #  pkg = "streamly";
          #  ver = "0.9.0";
          #  sha256 = "sha256-eOxVb8qQjZDo1+S7CStqYSExOg2QHWkMY+zlOYqwZak=";
          #} { };
          #          streamly-core = final.callHackageDirect {
          #            pkg = "streamly-core";
          #            ver = "0.1.0";
          #            sha256 = "sha256-hoSV6Q2+X5a7hFnJAArqNPjcMaCVyX9Vz4FcxeJ+jgI=";
          #          } { };
          #streamly-bytestring = final.callHackageDirect {
          #  pkg = "streamly-bytestring";
          #  ver = "0.2.1";
          #  sha256 = "sha256-EcH6qq4nRjea3xQ66Zlqgjjg7lF/grkKJI0+tTO4B84=";
          #} { };

        };
      myHaskellPackages = pkgs.haskell.packages.ghc948.extend overlay;

    in {
      #a = (pkgs.haskell.packages.ghc944.extend overlay).hscolour;
      defaultPackage.x86_64-linux = myHaskellPackages.project-m36;
      #packages.x86_64-linux.ghc944 =
      #  (pkgs.haskell.packages.ghc944.extend overlay).project-m36;
    };
}
