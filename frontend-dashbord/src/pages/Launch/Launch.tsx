import React from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import cx from "classnames";
import { LaunchDanaSwap, LaunchHeader } from "./sections";

import IMG_ScrollDown from "assets/icons/scroll-down.png";

const useStyles = makeStyles(({ palette }) => ({
  root: {},
  scroll: {
    position: "fixed",
    bottom: "10px",
    left: "calc(50vw - 30px)",
    cursor: "pointer",

    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "13px",
    lineHeight: "100%",
    textAlign: "center",
    color: "white",
  },
}));

const Launch: React.FC = () => {
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const dark = useIsDarkMode();
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <LaunchHeader />
      <LaunchDanaSwap />

      <Box className={cx(classes.scroll)}>
        <img src={IMG_ScrollDown} alt="scroll down" />
        <br />
        <br />
        <Box>
          Scroll down
        </Box>
      </Box>
    </Box>
  );
};

export default Launch;
