import React from "react";
import { Box } from "@material-ui/core";
import { AdSection, AssetSection, HelpSection } from "./sections";

const Landing: React.FC = () => {
  return (
    <Box>
      <AdSection />

      <Box mt='120px' />

      <AssetSection />

      <Box mt='150px' />

      <HelpSection />
    </Box>
  );
};

export default Landing;
