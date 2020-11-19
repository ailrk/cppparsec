# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # DON'T FORGET TO PUT YOUR PACKAGE NAME HERE, REMOVING `throw`
        crateName = "cppparsec";

      in {
        packages = {};

        devShell = pkgs.stdenvNoCC.mkDerivation {
          name = "shell";
          inputsFrom = builtins.attrValues self.packages.${system};
          buildInputs = [
            pkgs.gcc11
            pkgs.clang_13
            pkgs.cmake
            pkgs.cmakeCurses
            pkgs.gdb
            pkgs.gdbgui
            pkgs.catch2
            pkgs.ninja
          ];
        };
        LD_LIBRARY_PATH = [ "${pkgs.catch2}/lib" ];
      });
}
