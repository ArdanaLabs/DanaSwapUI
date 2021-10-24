import React from "react";
import cx from "classnames";
import { Box, makeStyles, useMediaQuery, useTheme } from "@material-ui/core";
import { useIsDarkMode } from "state/user/hooks";

import {
  MainSection,
  AboutSection,
  PartnerSection,
  DanaSwapSection,
  DanaTokenSection,
  StableCoinSection,
  InvestorsSection,
} from "./sections";

import BG_PURPLE_GRADIENT from "assets/backgrounds/pink-gradient.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  danaBG: {
    background: `url(${BG_PURPLE_GRADIENT}) top -200px left -700px no-repeat`,
  },
}));

const Landing: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box>
      <MainSection />
      <AboutSection />
      <StableCoinSection />
      <Box className={cx(classes.danaBG)}>
        <DanaSwapSection />
        <DanaTokenSection />
      </Box>
      <PartnerSection />
      <InvestorsSection />
    </Box>
  );
};

export default Landing;
