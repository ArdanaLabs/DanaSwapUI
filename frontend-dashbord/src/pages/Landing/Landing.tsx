import React from "react";
import { Box } from "@material-ui/core";

import {
  MainSection,
  PartnerSection,
  DanaSwapSection,
  StableCoinSection,
  TopNotchSection,
} from "./sections";

const Landing: React.FC = () => {
  return (
    <Box>
      <MainSection />
      <PartnerSection />
      <DanaSwapSection />
      <StableCoinSection />
      <TopNotchSection />
    </Box>
  );
};

export default Landing;
