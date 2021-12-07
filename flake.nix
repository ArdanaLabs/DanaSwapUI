{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    dream2nix = {
      url = "github:davhau/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat, flake-compat-ci, dream2nix }@inputs:
    let

      # Generate a user-friendly version number.
      version = builtins.substring 0 8 self.lastModifiedDate;

      # System types to support.
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlay ]; });

      dream2nix = inputs.dream2nix.lib.init {
        systems = supportedSystems;
        config = {
          overridesDirs = [ "${inputs.dream2nix}/overrides" ];
        };
      };

    in

    {
      overlay = final: prev: {};

      ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
      };

      packages = forAllSystems (system: {
        frontend-dashboard = (dream2nix.riseAndShine {
          source = ./frontend-dashbord;
        }).defaultPackage.${system};
        frontend-landing = (dream2nix.riseAndShine {
          source = ./frontend-landing;
        }).defaultPackage.${system};
        frontend-vault = (dream2nix.riseAndShine {
          source = ./frontend-vault;
        }).defaultPackage.${system};
      });
    };
}
