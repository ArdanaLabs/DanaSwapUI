import React from "react";
import { Box } from "@material-ui/core";

import {
  MainSection,
  AboutSection,
  PartnerSection,
  DanaSwapSection,
  StableCoinSection,
  InvestorsSection,
} from "./sections";

const Landing: React.FC = () => {
  return (
    <Box>
      <MainSection />
      <AboutSection />
      <StableCoinSection />
      <DanaSwapSection />
      <PartnerSection />
      <InvestorsSection />
    </Box>
  );
};

export default Landing;
