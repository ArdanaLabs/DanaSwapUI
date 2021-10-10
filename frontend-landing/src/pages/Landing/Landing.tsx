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
      <FooterSection />
    </Box>
  );
};

export default Landing;
