import React from "react";
import { Box } from "@material-ui/core";

import {
  MainSection,
  PartnerSection,
  DanaSwapSection,
  StableCoinSection,
  InvestorsSection,
} from "./sections";

const Landing: React.FC = () => {
  return (
    <Box>
      <MainSection />
      <StableCoinSection />
      <DanaSwapSection />
      <PartnerSection />
      <InvestorsSection />
    </Box>
  );
};

export default Landing;
