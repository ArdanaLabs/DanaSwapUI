import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";

import { useDarkModeManager, useIsDarkMode } from "state/user/hooks";

import ICON_SUN from "assets/image/icons/sun.svg";
import ICON_MOON from "assets/image/icons/moon.svg";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    width: "75px",
    cursor: "pointer",
    border: `2px solid ${palette.primary.main}`,
    filter: "drop-shadow(0px 4px 4px rgba(0, 0, 0, 0.25))",
    borderRadius: "100px",
    padding: "5px",
  },
  status: {
    width: "25px",
    height: "25px",
    background: palette.info.light,
    borderRadius: "100px",
    boxShadow: "0px 4px 4px rgba(0, 0, 0, 0.25)",
  },
}));

const ThemeSwitch: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const [darkMode, setDarkMode] = useDarkModeManager();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const toggleMode = () => {
    setDarkMode(!darkMode);
  };

  return (
    <Box
      display="flex"
      alignItems="center"
      justifyContent="space-between"
      className={cx(classes.root)}
      onClick={toggleMode}
    >
      <img src={dark ? ICON_SUN : ICON_MOON} alt="theme" />
      <Box className={cx(classes.status)} />
    </Box>
  );
};

export default ThemeSwitch;
