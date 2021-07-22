import React, { useState } from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import cx from "classnames";
import { LaunchTotalStats, LaunchHeader, LaunchPartialStats } from "./sections";

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

  const [nav, setNav] = useState(1);

  const updateNav = (newNav: number) => {
    setNav(newNav);
  };

  const handleScrollDown = () => {
    setTimeout(() => {
      setNav(0);
    }, 1000);
  };

  return (
    <Box className={cx(classes.root)}>
      <LaunchHeader nav={nav} updateNav={updateNav} />

      {nav === 0 && <LaunchPartialStats />}
      {nav === 1 && <LaunchTotalStats />}

      <Box className={cx(classes.scroll)} onClick={handleScrollDown}>
        <img src={IMG_ScrollDown} alt="scroll down" />
        <br />
        <br />
        <Box>Scroll down</Box>
      </Box>
    </Box>
  );
};

export default Launch;
