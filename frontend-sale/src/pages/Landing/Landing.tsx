import React from "react";
import cx from "classnames";
import {
  Box,
  Grid,
  makeStyles,
  useMediaQuery,
  useTheme,
} from "@material-ui/core";
import { useIsDarkMode } from "state/user/hooks";

import BG_GLOBE from "assets/backgrounds/globe.svg";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    padding: "100px 0px",
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontWeight: 900,
    fontSize: "80px",
    lineHeight: "100%",
    color: palette.text.primary,
  },
  comingSoon: {
    fontFamily: "Brandon Grotesque",
    fontWeight: 900,
    fontSize: "45px",
    lineHeight: "79px",
    paddingTop: "20px",
    color: "#5291C7",
  },
}));

const Landing: React.FC = () => {
  const theme = useTheme();
  const mobile = useMediaQuery(theme.breakpoints.down("sm"));
  const dark = useIsDarkMode();
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <Grid container alignItems="center">
        <Grid item xs={12} sm={6}>
          <Box className={cx(classes.title)}>
            Ardana Public
            <br />
            Token Sale
          </Box>
          <Box className={cx(classes.comingSoon)}>COMING SOON.</Box>
        </Grid>
        <Grid item xs={12} sm={6}>
          <Box textAlign="center">
            <img src={BG_GLOBE} alt="globe" />
          </Box>
        </Grid>
      </Grid>
    </Box>
  );
};

export default Landing;
