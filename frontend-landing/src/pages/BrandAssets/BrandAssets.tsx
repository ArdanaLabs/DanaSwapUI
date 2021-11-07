import React from "react";
import { Box } from "@material-ui/core";

import { GuideLinesSection, MainSection } from "./sections";

const BrandAssets: React.FC = () => {
  return (
    <Box>
      <MainSection />
      <GuideLinesSection />
    </Box>
  );
};

export default BrandAssets;
