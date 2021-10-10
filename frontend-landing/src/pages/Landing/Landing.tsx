import React from "react";
import { Box } from "@material-ui/core";

import {
  MainSection,
  PartnerSection,
  DanaSwapSection,
  StableCoinSection,
  TopNotchSection,
  // ProfileSection,
  FooterSection,
  AdvisorsSection,
  InvestorsSection,
} from "./sections";

const Landing: React.FC = () => {
  return (
    <Box>
      <MainSection />
      <StableCoinSection />
      <DanaSwapSection />
      <TopNotchSection />
      {/* <ProfileSection /> */}
      <AdvisorsSection />
      <PartnerSection />
      <InvestorsSection />
      <FooterSection />
    </Box>
  );
};

export default Landing;
