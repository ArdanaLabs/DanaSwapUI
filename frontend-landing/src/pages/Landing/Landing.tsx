import React from "react";
import { Box } from "@material-ui/core";
import { AdSection, AssetSection, HelpSection } from "./sections";

const Landing: React.FC = () => {
  return (
    <Box>
      <AdSection />
      {/* <AssetSection /> */}
      <HelpSection />
    </Box>
  );
};

export default Landing;
