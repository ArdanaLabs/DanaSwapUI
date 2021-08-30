import React from "react";
import cx from "classnames";
import { Box } from "@material-ui/core";
import { makeStyles } from "@material-ui/core/styles";

import { useDarkModeManager } from "state/user/hooks";

import ICON_SUN from "assets/image/ICON-SUN.png";
import ICON_MOON from "assets/image/ICON-MOON.png";

const useStyles = makeStyles(({ palette }) => ({
  root: {
    margin: "0 10px",
    padding: '0px 15px',
    cursor: "pointer",
    borderRadius: "100px",
    display: "flex",
    alignItems: "center",
    width: "175px",
    background: palette.info.light,
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
  const [darkMode, setDarkMode] = useDarkModeManager();
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
      <Box className={cx(classes.switchLabel)}>
        {!darkMode ? "DARKMODE" : "LIGHTMODE"}
      </Box>
    </Box>
  );
};

export default ThemeSwitch;
