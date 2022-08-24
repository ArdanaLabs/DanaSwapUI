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
      dream2nixAllSystems = forAllSystems (
        system: pkgs:
          inputs.dream2nix.lib.init {
            inherit pkgs;
            config = {
              projectRoot = ./.;
              overridesDirs = [ "${inputs.dream2nix}/overrides" ./dream2nix-overrides ];
            };
          }
      );
      dream2nixForSystem = system: dream2nixAllSystems.${system};
      projects = forAllSystems (
        system: pkgs:
          let
            d2n = dream2nixForSystem system;
            makeProject = src:
              d2n.makeOutputs {
                source = src;
                settings = [{ subsystemInfo.nodejs = 16; }];
              };
          in
          {
            ardana-application = makeProject ./frontend-dashboard;
            ardana-landing = makeProject ./frontend-landing;
            ardana-vault = makeProject ./frontend-vault;
            lighthouse = makeProject lighthouse-src;
          }
      );
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
              productionMentions = "<@591177767467614238>";
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

      packages = forAllSystems (
        system: pkgs:
          let
            getPackage = name:
              projects.${system}.${name}.packages.${name};
          in
          {
            ardana-application = getPackage "ardana-application";
            ardana-landing = getPackage "ardana-landing";
            ardana-vault = getPackage "ardana-vault";
            lighthouse = getPackage "lighthouse";
          }
      );

      devShells = forAllSystems (
        system: pkgs: {
          default =  pkgs.mkShell {
            name = "DanaSwapUI";
            buildInputs = with pkgs; [
              nodejs-16_x
            ];
            shellHook = ''
              export PATH="$PWD/node_modules/.bin/:$PATH"
            '';
          };
        }
      );
      
      devShell = forAllSystems (
        system: pkgs: self.devShells.${system}.default
      );
    };
}
