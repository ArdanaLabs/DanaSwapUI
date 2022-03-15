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

      dream2nix = inputs.dream2nix.lib2.init {
        systems = supportedSystems;
        config = {
          overridesDirs = [ "${inputs.dream2nix}/overrides" ];
          projectRoot = ./.;
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
         robotsTxt = builtins.toFile "robots.txt" ''
           User-agent: *
           Disallow: /
         '';
         # TODO: put mkWebsite and mkGitBranchViaEffect into a library such as
         # danalib or hercules-ci-effects itself.
         mkWebsite = args: mkGitBranchViaEffect (args // {
           gitRemote = "git@codeberg.org";
           hostKey = "codeberg.org ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIVIC02vnjFyL+I4RHfvIGNtOgJMe769VTF1VR4EB3ZB";
           owner = "ardanalabs";
           repo = "pages";
           committerEmail = "matthew.croughan@platonic.systems";
           committerName = "Hercules-CI Effects";
           authorName = "Hercules-CI Effects";
         });
         mkGitBranchViaEffect = { gitRemote, hostKey, triggerBranch, pushToBranch, owner, repo, branchRoot, committerEmail, committerName, authorName, preGitInit ? "", ... }:
           hci-effects.runIf (src.ref == "refs/heads/${triggerBranch}") (
             hci-effects.mkEffect {
               src = self;
               buildInputs = with pkgs; [ openssh git ];
               secretsMap = {
                 "ssh" = "ssh";
               };
               effectScript =
               ''
                 writeSSHKey
                 echo ${hostKey} >> ~/.ssh/known_hosts
                 export GIT_AUTHOR_NAME="${authorName}"
                 export GIT_COMMITTER_NAME="${committerName}"
                 export EMAIL="${committerEmail}"
                 cp -r --no-preserve=mode ${branchRoot} ./${pushToBranch} && cd ${pushToBranch}
                 ${preGitInit}
                 git init -b ${pushToBranch}
                 git remote add origin ${gitRemote}:${owner}/${repo}.git
                 git add .
                 git commit -m "Deploy to ${pushToBranch}"
                 git push -f origin ${pushToBranch}:${pushToBranch}
               '';
             }
           );
       in
       {
         # TODO: Create a function that creates staging/production from our
         # existing packages attribute, which also templates branchRoot like
         # node_modules/ardana-landing -> node_modules/${packageName}
         # TODO: Remove recurseIntoAttrs, it won't be needed in a later Hercules
         # version.
         staging = nixpkgs.lib.recurseIntoAttrs {
           frontend-landing = mkWebsite {
             branchRoot = "${self.packages.${system}.frontend-landing}/lib/node_modules/ardana-landing/build";
             triggerBranch = "staging";
             pushToBranch = "staging-frontend-landing";
             preGitInit = ''
               cat ${robotsTxt} > robots.txt
               echo "staging-frontend-landing-dc1ece41.ardana.org" > .domains
             '';
           };
           frontend-dashboard = mkWebsite {
             branchRoot = "${self.packages.${system}.frontend-dashboard}/lib/node_modules/ardana-application/build";
             triggerBranch = "staging";
             pushToBranch = "staging-frontend-dashboard";
             preGitInit = ''
               cat ${robotsTxt} > robots.txt
               echo "staging-frontend-dashboard-dc1ece41.ardana.org" > .domains
             '';
           };
           frontend-vault = mkWebsite {
             branchRoot = "${self.packages.${system}.frontend-vault}/lib/node_modules/ardana-vault/build";
             triggerBranch = "staging";
             pushToBranch = "staging-frontend-vault";
             preGitInit = ''
               cat ${robotsTxt} > robots.txt
               echo "staging-frontend-vault-dc1ece41.ardana.org" > .domains
             '';
           };
         };
         production = nixpkgs.lib.recurseIntoAttrs {
           frontend-landing = mkWebsite {
             branchRoot = "${self.packages.${system}.frontend-landing}/lib/node_modules/ardana-landing/build";
             triggerBranch = "main";
             pushToBranch = "production-frontend-landing";
             preGitInit = ''
               echo "ardana.org" > .domains
             '';
           };
           frontend-dashboard = mkWebsite {
             branchRoot = "${self.packages.${system}.frontend-dashboard}/lib/node_modules/ardana-application/build";
             triggerBranch = "main";
             pushToBranch = "production-frontend-dashboard";
             preGitInit = ''
               echo "dashboard.ardana.org" > .domains
             '';
           };
           frontend-vault = mkWebsite {
             branchRoot = "${self.packages.${system}.frontend-vault}/lib/node_modules/ardana-vault/build";
             triggerBranch = "main";
             pushToBranch = "production-frontend-vault";
             preGitInit = ''
               echo "vault.ardana.org" > .domains
             '';
           };
         };
       };

      packages = forAllSystems (system: {
        frontend-dashboard = (dream2nix.makeFlakeOutputs {
          source = ./frontend-dashboard;
        }).packages.${system}.ardana-application;
        frontend-landing = (dream2nix.makeFlakeOutputs {
          source = ./frontend-landing;
        }).packages.${system}.ardana-landing;
        frontend-vault = (dream2nix.makeFlakeOutputs {
          source = ./frontend-vault;
        }).packages.${system}.ardana-vault;
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
