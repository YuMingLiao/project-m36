{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

  outputs = { self, nixpkgs }:
    let
      config = {
        allowBroken = true;
        allowAliases = false;
      };
      pkgs =
        nixpkgs.legacyPackages.x86_64-linux.extend (f: p: { inherit config; });
      overlay = with pkgs.haskell.lib;
        with pkgs.lib.fileset;
        final: prev: {
          # `nix flake show` and `nix flake check` won't work because of
          # IFD (https://nixos.wiki/wiki/Import_From_Derivation).
          # Might be possible to use in the near future, check this out:
          # https://github.com/cdepillabout/cabal2nixWithoutIFD
          mkDerivation = drv:
            prev.mkDerivation (drv // {
              # doCheck = false;
              # doHaddock = false;
              # enableLibraryProfiling = false;
              # enableExecutableProfiling = false;
            });
          project-m36 = let
            source = toSource {
              root = ./.;
              fileset = ./.git; # so that specific rev won't get rebuild due to unstaged files change.
            };
          in final.callCabal2nix "project-m36" source { };
          barbies-th = doJailbreak prev.barbies-th;
          curryer-rpc = final.callHackageDirect {
            pkg = "curryer-rpc";
            ver = "0.3.5";
            sha256 = "sha256-7mEJOBKzA2rTnLxZme8E6zFv0VkiXBo5L/jUJSNPaNE=";
          } { };
          streamly = final.callHackageDirect {
            pkg = "streamly";
            ver = "0.9.0";
            sha256 = "sha256-eOxVb8qQjZDo1+S7CStqYSExOg2QHWkMY+zlOYqwZak=";
          } { };
          streamly-core = final.callHackageDirect {
            pkg = "streamly-core";
            ver = "0.1.0";
            sha256 = "sha256-hoSV6Q2+X5a7hFnJAArqNPjcMaCVyX9Vz4FcxeJ+jgI=";
          } { };
          streamly-bytestring = final.callHackageDirect {
            pkg = "streamly-bytestring";
            ver = "0.2.1";
            sha256 = "sha256-EcH6qq4nRjea3xQ66Zlqgjjg7lF/grkKJI0+tTO4B84=";
          } { };

        };

      myHaskellPackages = pkgs.haskellPackages.extend overlay;
    in {
      defaultPackage.x86_64-linux = myHaskellPackages.project-m36;

    };
}
# Define apps that is triggered by `nix run` command. For example,
# `nix run .#postgres` will run the script for postgres below
#      apps = forAllSystems (system: {
#        postgres =
#          {
# `type` and `program` are required attributes.
# The type attribute determines how the program should be executed, For example, "shell" for a shell script,
# "python" for a Python script, or "app" for an executable.
# `program` denotes the path of the executable to run
#            type = "app";
#            program =
#              let
#                script = pkgs.${system}.writeShellApplication {
#                  name = "pg_start";
#                  runtimeInputs = [ pkgs.${system}.postgresql ];
#                  text =
#                    ''
#                      # Initialize a database with data stored in current project dir
#                      [ ! -d "./data/db" ] && initdb --no-locale -D ./data/db
#
#                      postgres -D ./data/db -k "$PWD"/data
#                    '';
#                };
#              in
#              "${script}/bin/pg_start";
#          };
#        createdb = {
#          type = "app";
#          program =
#            let
#              script = pkgs.${system}.writeShellApplication {
#                name = "create_db";
#                runtimeInputs = [ pkgs.${system}.postgresql ];
#                text =
#                  ''
#                    # Create a database of your current user
#                    if ! psql -h "$PWD"/data -lqt | cut -d \| -f 1 | grep -qw "$(whoami)"; then
#                      createdb -h "$PWD"/data "$(whoami)"
#                    fi
#
#                    # Load DB dump
#                    # TODO: check if schema already exists
#                    psql -h "$PWD"/data < db.sql
#                
#                    # Create configuration file for postgrest
#                    echo "db-uri = \"postgres://authenticator:mysecretpassword@localhost:5432/$(whoami)\"
#                    db-schemas = \"api\"
#                    db-anon-role = \"todo_user\"" > data/db.conf
#                  '';
#              };
#            in
#            "${script}/bin/create_db";
#        };
#
#        postgrest = {
#          type = "app";
#          program =
#            let
#              script = pkgs.${system}.writeShellApplication {
#                name = "pg_rest";
#                runtimeInputs = [ myHaskellPackages.${system}.postgrest ];
#                text =
#                  ''
#                    postgrest ./data/db.conf
#                  '';
#              };
#            in
#            "${script}/bin/pg_rest";
#        };
#      });

