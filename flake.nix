{
  inputs = rec {
    # from niv
    # nixpkgs.url = "https://github.com/NixOS/nixpkgs/archive/9fafaa30660e204bb27a35b3c608f03609705a5d.tar.gz";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
   streamly-bytestring = {
      type = "github";
      owner = "psibi";
      repo = "streamly-bytestring";
      flake = false;
    };
    scotty.url = "github:scotty-web/scotty";
    scotty.flake = false;
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      streamly-bytestring,
      scotty,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        {
          self',
          pkgs,
          config,
          ...
        }:
        {

          # Typically, you just want a single project named "default". But
          # multiple projects are also possible, each using different GHC version.
          haskellProjects.ghc965 = {
            defaults.packages = { }; # no local packages
            devShell.enable = false;
            autoWire = [];
            basePackages = pkgs.haskell.packages.ghc965;
            packages =
              let
                self = pkgs.haskell.packages.ghc965;
              in
              {
                streamly-bytestring.source = streamly-bytestring;
                scotty.source = scotty;
              };
            settings = {
              streamly-bytestring.jailbreak = true;
              streamly.jailbreak = true;
              curryer-rpc.jailbreak = true;
            };
          };

          haskellProjects.default = {
            # The base package set representing a specific GHC version.
            # By default, this is pkgs.haskellPackages.
            # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
            basePackages = config.haskellProjects.ghc965.outputs.finalPackages;

            # Extra package information. See https://community.flake.parts/haskell-flake/dependency
            #
            # Note that local packages are automatically included in `packages`
            # (defined by `defaults.packages` option).
            #defaults.packages = {};
            packages = {
              # aeson.source = "1.5.0.0";      # Override aeson to a custom version from Hackage
              # shower.source = inputs.shower; # Override shower to a custom source path
            };
            settings = {
              #  aeson = {
              #    check = false;
              #  };
              #  relude = {
              #    haddock = false;
              #    broken = false;
              #  };
            };

            devShell = {
              # Enabled by default
              # enable = true;

              # Programs you want to make available in the shell.
              # Default programs can be disabled by setting to 'null'
              # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };

              # Check that haskell-language-server works
              # hlsCheck.enable = true; # Requires sandbox to be disabled
            };
          };

          # haskell-flake doesn't set the default package, but you can do it here.
          # packages.default = config.haskellProjects.ghc948.outputs.finalPackages.lattices;
          packages.default = self'.packages.project-m36;
        };
    };
}
