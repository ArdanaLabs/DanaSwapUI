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
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
  };

  outputs = { self, nixpkgs, flake-compat, flake-compat-ci, dream2nix, hercules-ci-effects }@inputs:
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
      overlay = final: prev: { };

      ciNix = args@{ src }: flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
        effectsArgs = args;
      };

     effects = { src }:
       let
         pkgs = nixpkgs.legacyPackages.x86_64-linux;
         hci-effects = hercules-ci-effects.lib.withPkgs pkgs;
         system = "x86_64-linux";
       in
       {
         gh-pages = hci-effects.runIf (src.ref == "refs/heads/main") (
           hci-effects.mkEffect {
             src = self;
             buildInputs = with pkgs; [ openssh git ];
             secretsMap = {
               "ssh" = "ssh";
             };
             effectScript =
             let
               CNAME = "ardana.org";
               githubHostKey = "github.com ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
             in
             ''
               export WEBSITE="${self.packages.${system}.frontend-landing}/lib/node_modules/ardana-landing/build"
               writeSSHKey
               echo ${githubHostKey} >> ~/.ssh/known_hosts
               export GIT_AUTHOR_NAME="Hercules-CI Effects"
               export GIT_COMMITTER_NAME="Hercules-CI Effects"
               export EMAIL="github@croughan.sh"
               cp -r --no-preserve=mode "$WEBSITE" ./gh-pages && cd gh-pages
               echo "${CNAME}" > CNAME
               git init -b gh-pages
               git remote add origin git@github.com:ArdanaLabs/DanaSwapUI.git
               git add .
               git commit -m "Deploy to gh-pages"
               git push -f origin gh-pages:gh-pages
             '';
           }
         );
       };

      packages = forAllSystems (system: {
        frontend-dashboard = (dream2nix.riseAndShine {
          source = ./frontend-dashboard;
        }).defaultPackage.${system};
        frontend-landing = (dream2nix.riseAndShine {
          source = ./frontend-landing;
        }).defaultPackage.${system};
        frontend-vault = (dream2nix.riseAndShine {
          source = ./frontend-vault;
        }).defaultPackage.${system};
      });

      devShell = forAllSystems (system:
        let
          pkgs = nixpkgsFor."${system}";
        in
        pkgs.mkShell {
          name = "DanaSwapUI";
          buildInputs = with pkgs; [
            nodejs-16_x
          ];
          shellHook = ''
            export PATH="$PWD/node_modules/.bin/:$PATH"
          '';
        });
    };
}
