import React, { useState } from "react";
import { Box, Container, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import cx from "classnames";
import IMG_logo from "assets/logos/Ardana_hor_white.png";
import { useHistory } from "react-router-dom";

const useStyles = makeStyles(({ palette }) => ({
  header: {
    cursor: "pointer",
    position: "fixed",
    top: "0",
    width: "100%",
    zIndex: 1000,

    "& > div": {
      display: "flex",
      justifyContent: "space-between",
      alignItems: "center",
    },
  },

  navGroup: {
    display: "flex",
    justifyContent: "center",
  },
  navItem: {
    margin: "10px",
    padding: "10px 30px",
    borderRadius: "20px",
    background: "transparent",
    color: "white",
    textAlign: "center",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: "11px",
    lineHeight: "100%",
    border: "1px solid white",
    cursor: "pointer",

    "&:hover": {
      background: "white",
      color: "#000633",
    },
  },

  active: {
    border: "unset",
    background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
    "&:hover": {
      background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
    },
  },
}));

const LaunchHeader: React.FC = () => {
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const dark = useIsDarkMode();
  const classes = useStyles({ dark, mobile });
  const history = useHistory();

  const [nav, setNav] = useState(1);

  return (
    <Box className={cx(classes.header)}>
      <Container>
        <Box onClick={() => history.push("/")}>
          <img src={IMG_logo} alt="Ardana Logo" />
        </Box>
        <Box className={cx(classes.navGroup)}>
          <Box
            className={cx(classes.navItem, {
              [classes.active]: nav === 0,
            })}
            onClick={() => setNav(0)}
          >
            LAUNCH ARDANA STABLECOINS
          </Box>
          <Box
            className={cx(classes.navItem, {
              [classes.active]: nav === 1,
            })}
            onClick={() => setNav(1)}
          >
            LAUNCH DANASWAP
          </Box>
          <Box
            className={cx(classes.navItem, {
              [classes.active]: nav === 2,
            })}
            onClick={() => setNav(2)}
          >
            MY DASHBOARD
          </Box>
        </Box>
      </Container>
    </Box>
  );
};

export default LaunchHeader;
