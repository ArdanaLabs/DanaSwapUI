import React from "react";
import { Box } from "@material-ui/core";

import { HeroSection, MapSection } from "./sections";

const RoadMap: React.FC = () => {
  return (
    <Box>
      <HeroSection />
      <MapSection />
    </Box>
  );
};

export default RoadMap;
