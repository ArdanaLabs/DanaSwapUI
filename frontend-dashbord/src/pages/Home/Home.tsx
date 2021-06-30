import React from "react";
import cx from "classnames";
import { Box, Container, Grid, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

import { OverViewSection, ChartSection } from "./sections";

const useStyles = makeStyles(({ palette }) => ({
  self: {
  },
}));

const Home: React.FC = () => {
  const { palette, breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.self)}>
      <OverViewSection />
      
      <Box mt="30px" />

      <ChartSection />

    </Box>
  );
};

export default Home;
