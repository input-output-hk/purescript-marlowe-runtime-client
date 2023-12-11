{
  # nixConfig = {
  #   bash-prompt-suffix = "[dev]";
  # };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flakeUtils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    easyPSSrc = {
      flake = false;
      url = "github:justinwoo/easy-purescript-nix/d5fe5f4b210a0e4bac42ae0c159596a49c5eb016";
    };
  };

  outputs = { self, nixpkgs, flakeUtils, easyPSSrc, ... }:
    flakeUtils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        easyPS = pkgs.callPackage easyPSSrc { inherit pkgs; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = [
            # Please update spago and purescript in `package.json` `scripts` section
            easyPS."purs-0_15_10"
            easyPS."purs-tidy"
            easyPS.purescript-language-server
            easyPS.pscid
            # easyPS.purs-tidy
            easyPS.pulp
            easyPS.spago

            pkgs.jq
            pkgs.dhall
            pkgs.pkg-config
            pkgs.unzip
        ];
        shellHook = ''
          export PS1="\n\[\033[1;32m\][nix develop:\w]\$\[\033[0m\] ";
        '';
      };
    }
  );
}
