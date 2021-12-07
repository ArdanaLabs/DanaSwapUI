import React from "react";
import { Box } from "@material-ui/core";

import { GuideLinesSection, MainSection, PressKitSection } from "./sections";

const BrandAssets: React.FC = () => {
  return (
    <Box>
      <MainSection />
      <GuideLinesSection />
      <PressKitSection />
    </Box>
  );
};

export default BrandAssets;
