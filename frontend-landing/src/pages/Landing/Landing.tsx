import React from "react";
import { Box } from "@material-ui/core";

import {
  MainSection,
  AboutSection,
  PartnerSection,
  DanaSwapSection,
  DanaTokenSection,
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
      <DanaTokenSection />
      <PartnerSection />
      <InvestorsSection />
    </Box>
  );
};

export default Landing;
