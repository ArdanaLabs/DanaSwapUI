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
    background: palette.type === 'light' ? 'linear-gradient(89.62deg, #000A4F 0.3%, #3C4DC5 99.64%)' : 'linear-gradient(89.62deg, #72D2F2 0.3%, #6077FF 99.64%)',
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
    lineHeight: "16px",
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
