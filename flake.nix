{ inputs =
    { npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
      # ^ was unable to get this to work at all
      nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

      npmlock2nix-src =
        { flake = false;
          url = "path:/home/mason/git/npmlock2nix";
          # changing this to `ref = "master"` was required to fix an issue https://github.com/nix-community/npmlock2nix/blob/33eb3300561d724da64a46ab8e9a05a9cfa9264b/internal.nix#L157
          # url = "github:nix-community/npmlock2nix";
          # url = "github:Sohalt/npmlock2nix/b5ef1a09b37d5544961c2052a3ed03b2246ea487";
          # ^ this can handle git+https links but only if they include a `#rev` at the end - not all of ours do
        };

      utils.url = "github:ursi/flake-utils/2";
    };

  outputs = { nixpkgs, npm-buildpackage, npmlock2nix-src, utils, ... }@inputs:
    with builtins;
    utils.default-systems
      ({ make-shell, system, ... }:
         let
           l = p.lib; p = pkgs;

           pkgs =
             import nixpkgs
               { overlays = [ npm-buildpackage.overlay ];
                 inherit system;
               };

           npmlock2nix = import npmlock2nix-src { inherit pkgs; };

           # it can't handle git+https links so I change them to github:org/repo#rev
           lock-with-fixed-urls =
             p.writeText "package-lock.json"
               (toJSON
                  (l.mapAttrsRecursive
                     (path: value:
                        let
                          git-url-bits =
                            match
                              ''git\+https://github.com/([^/]+)/([^.]+)[^#]*#?(.+)?''
                              value;
                        in
                        if isString value && git-url-bits != null then
                          let
                            log = a: trace a a;
                            org = head git-url-bits;
                            repo = head (tail git-url-bits);
                            version = head (tail (tail git-url-bits));
                          in
                          "github:${org}/${repo}"
                          + (if version != null then "#${version}" else
                          # "#master"
                          "#ee3994657fa7a427238e6ba92a84d0b529bbcde0"
                          )
                        else
                          value
                     )
                     (l.importJSON ./frontend-landing/package-lock.json)
                  )
               );

           patched-src =
             let
             in
             p.stdenv.mkDerivation
               { name = "patched-src";
                 src = ./frontend-landing;

                 patchPhase =
                   ''
                   cp ${lock-with-fixed-urls} package-lock.json
                   '';

                installPhase = "cp -r ./. $out";
               };
         in
         { defaultPackage = p.writeText "" ":(";
             /* nod2nix attempt

             p.stdenv.mkDerivation
               { name = "miracle";

                 src =
                   (import ./frontend-landing { inherit pkgs; }).package
                   + /lib/node_modules/premia-interface;

                 buildInputs = with p; [ nodejs ];

                 installPhase =
                   ''
                   cp -r ./. $out
                   cd $out; npm run build
                   '';
               };
             */

             /* npmlock2nix attempt

             npmlock2nix.node_modules
               { src = patched-src;
               };
             */

           devShell =
             make-shell
               { packages =
                   with pkgs;
                   [ nodejs
                     nodePackages.node2nix
                   ];
               };
         }
      )
      inputs;
}
