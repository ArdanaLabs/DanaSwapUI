import React from "react";
import { Box, useMediaQuery, useTheme } from "@material-ui/core";
import {
  AdSection,
  AssetSection,
  HelpSection,
  FeatureSection,
} from "./sections";

const Landing: React.FC = () => {
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));

  return (
    <Box>
      <AdSection />

      <Box mt={!mobile ? "120px" : "30px"} />

      <FeatureSection />

      <Box mt={!mobile ? "120px" : "30px"} />

      <AssetSection />

      <Box mt={!mobile ? "150px" : "100px"} />

      <HelpSection />
    </Box>
  );
};

export default Landing;
