{ nixosTest
, ardana-landing # The ardana-landing website
, lighthouse # The lighthouse executable
, categories ? { performance = 0.9; accessibility = 0.95; seo = 0.99; best-practices = 0.80; } # The minimal score that needs to be reached for each category
}:
let
  name = "ardana-landing-lighthouse";
  ardana-server-port = "8000";
in
nixosTest {
  inherit name;

  nodes = {
    server = { config, pkgs, ... }: {
      users.users.ardana-landing = {
        name = "ardana-landing";
        group = "ardana-landing";
        isSystemUser = true;
      };
      users.groups.ardana-landing = { };

      systemd.services.ardana-landing-server = {
        wantedBy = [ "multi-user.target" ]; 
        after = [ "network.target" ];
        description = "ardana-landing-server";
        serviceConfig = {
          User = "ardana-landing";
          Group = "ardana-landing";
          ExecStart = ''${pkgs.simple-http-server}/bin/simple-http-server -c=js,css,svg,html -i -p ${ardana-server-port} -- ${ardana-landing}'';
        };
      };
    };
  };
  testScript = ''
    import json
    import os

    start_all()
    server.wait_for_unit("network.target")
    server.wait_for_open_port(${ardana-server-port})

    report_path = "/tmp/lighthouse-report.json"
    server.succeed("CI=1 ${lighthouse}/bin/lighthouse http://localhost:${ardana-server-port} --output json --output-path {} --only-categories accessibility,best-practices,performance,seo --skip-audits valid-source-maps --chrome-flags=\"--headless --no-sandbox\"".format(report_path))
    server.copy_from_vm(report_path)

    with open("{}/lighthouse-report.json".format(os.environ["out"]), "r") as f:
      report = json.load(f)
      categories = report["categories"]
      assert categories["performance"]["score"]    >= ${toString categories.performance}, "performance score should be at least ${toString (categories.performance * 100)}%"
      assert categories["accessibility"]["score"]  >= ${toString categories.accessibility}, "accessibility score should be at least ${toString (categories.accessibility * 100)}%"
      assert categories["seo"]["score"]            >= ${toString categories.seo}, "seo score should be at least ${toString (categories.seo * 100)}%"
      assert categories["best-practices"]["score"] >= ${toString categories.best-practices}, "best-practices score should be at least ${toString (categories.best-practices * 100)}%"
  '';
}
