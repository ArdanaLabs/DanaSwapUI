{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    dream2nix = {
      url = "github:davhau/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    lighthouse-src = {
      url = "github:GoogleChrome/lighthouse/v9.5.0";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, dream2nix, hercules-ci-effects, lighthouse-src }@inputs:
    let
      supportedSystems = [ "x86_64-linux" ];
      forSystems = systems: f:
        nixpkgs.lib.genAttrs systems
        (system: f system nixpkgs.legacyPackages.${system});
      forAllSystems = forSystems supportedSystems;
      dream2nix = inputs.dream2nix.lib2.init {
        systems = supportedSystems;
        config = {
          projectRoot = ./.;
          overridesDirs = [ "${inputs.dream2nix}/overrides" ./dream2nix-overrides ];
        };
      };
    in
    {
      effects = { branch, rev, ... }:
        let
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          hci-effects = hercules-ci-effects.lib.withPkgs pkgs;
          system = "x86_64-linux";
          mkWebsite = projectName: siteId:
            let
              productionMessage = "${projectName} has been deployed for **production**";
              normalMessage = "${projectName} has been deployed for preview";
              productionMentions = "<@558450198670475294>";
              normalMentions = "<@685818055544012885>";
              message = if (branch == "main") then productionMessage else normalMessage;
              mentions = if (branch == "main") then productionMentions else normalMentions;
              discord-sh = (builtins.getFlake "github:matthewcroughan/nixpkgs/b96b41d2a818c4b997b8e6a647b960a01c7f046c").legacyPackages.${system}.discord-sh;
            in
            hci-effects.netlifyDeploy {
              productionDeployment = (branch == "main");
              content = "${self.packages.${system}.${projectName}}/lib/node_modules/${projectName}/build";
              secretName = "default-netlify";
              secretField = "authToken";
              siteId = siteId;
              secretsMap."ardanaDiscord" = "ardanaDiscord";
              postEffect = ''
                readSecretString ardanaDiscord .webhook > .webhook
                ${discord-sh}/bin/discord.sh \
                  --description "${message}" \
                  --field "Deploy URL;$(jq -r '.deploy_url' netlify-result.json)" \
                  --field "Branch;${branch}" \
                  --field "Commit ID;${rev}" \
                  --text "${mentions}"
              '';
            };
        in
        {
          ardana-application = mkWebsite
            "ardana-application"
            "c564a7b1-3ee6-4755-b046-af37aa998ab1";
          ardana-landing = mkWebsite
            "ardana-landing"
            "28e10575-cd5f-4e09-97b5-bd8f65c006ab";
          ardana-vault = mkWebsite
            "ardana-vault"
            "bdcb97a1-e22e-49b4-ac7f-0b45aa2c35da";
        };

      packages = forAllSystems (system: pkgs: {
        ardana-application = (dream2nix.makeFlakeOutputs {
          source = ./frontend-dashboard;
        }).packages.${system}.ardana-application;
        ardana-landing = (dream2nix.makeFlakeOutputs {
          source = ./frontend-landing;
        }).packages.${system}.ardana-landing;
        ardana-vault = (dream2nix.makeFlakeOutputs {
          source = ./frontend-vault;
        }).packages.${system}.ardana-vault;
        lighthouse = (dream2nix.makeFlakeOutputs {
          source = lighthouse-src;
        }).packages.${system}.lighthouse;
      });

      devShell = forAllSystems (system: pkgs:
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
