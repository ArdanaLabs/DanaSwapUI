import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";

import { useDarkModeManager } from "state/user/hooks";

import ICON_SUN from "assets/image/ICON-SUN.png";
import ICON_MOON from "assets/image/ICON-MOON.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    margin: "0 10px",
    padding: '0px 15px',
    cursor: "pointer",
    borderRadius: "100px",
    display: "flex",
    alignItems: "center",
    width: "175px",
    background: palette.info.light,

    [breakpoints.down('sm')]: {
      width: 'auto',
      padding: 5,
      display: 'inline-flex',
      lineHeight: '100%',
      boderRadius: '50%',
    }
  },
  switchIcon: {
    padding: 5,
    width: 28,
    height: 28,
  },
  switchLabel: {
    fontSize: "14px",
    fontWeight: 700,
    fontFamily: "Museo Sans",
    color: "#FFFFFF",
    flexGrow: 5,
    textAlign: "center",
  },
}));

const ThemeSwitch: React.FC = () => {
  const { breakpoints } = useTheme()
  const [darkMode, setDarkMode] = useDarkModeManager();
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles();

  const toggleMode = () => {
    setDarkMode(!darkMode);
  };

  return (
    <Box
      className={cx(classes.root)}
      onClick={toggleMode}
    >
      <img
        className={cx(classes.switchIcon)}
        src={!darkMode ? ICON_MOON : ICON_SUN}
        alt="Theme switch icon"
      />
      {!mobile && (<Box className={cx(classes.switchLabel)}>
        {!darkMode ? "DARKMODE" : "LIGHTMODE"}
      </Box>)}
    </Box>
  );
};

export default ThemeSwitch;
