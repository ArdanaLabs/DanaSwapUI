import React, { useState } from "react";
import { Box, Container, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import cx from "classnames";
import { SwitchWithGlider } from "components";
import IMG_logo from "assets/logos/Ardana_hor_white.png";
import IMG_bg from "assets/backgrounds/launch-bg.png";
import { useHistory } from "react-router-dom";

const useStyles = makeStyles(({ palette }) => ({
  root: {
    background: `linear-gradient(179.02deg, #000633 26.91%, rgba(0, 5, 38, 0.5) 99.11%), url(${IMG_bg})`,
    mixBlendMode: "normal",
    minHeight: "100vh",
  },
  header: {
    cursor: "pointer",
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "50px",
    lineHeight: "120.5%",
    textAlign: "center",
    color: "#FFFFFF",
    padding: "30px",
  },

  navGroup: {
    padding: "30px",
  },
  navItem: {
    borderRadius: "20px",
    width: "170px",
    background: "transparent",
    padding: "10px",
    color: "white",
    textAlign: "center",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: "13px",
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

  statGroup: {
    display: "flex",
    justifyContent: "center",
    marginTop: "50px",
  },

  statBox: {
    width: "200px",
    margin: "20px",
    padding: "10px 30px 30px 30px",
    background:
      "linear-gradient(0deg, rgba(47, 61, 160, 0.6), rgba(47, 61, 160, 0.6))",
    borderRadius: "10px",
    color: "white",
    textAlign: "center",
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "36px",
    lineHeight: "110%",

    "& > span": {
      fontFamily: "Museo Sans",
      fontSize: "13px",
      lineHeight: "16px",
      fontWeight: 300,
    },
  },
}));

const Launch: React.FC = () => {
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const dark = useIsDarkMode();
  const classes = useStyles({ dark, mobile });
  const history = useHistory();

  const [nav, setNav] = useState(1);

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box className={cx(classes.header)} onClick={() => history.push("/")}>
          <img src={IMG_logo} alt="Ardana Logo" />
        </Box>

        <Box className={cx(classes.title)}>
          Access to your resources,
          <br />
          all in one place.
        </Box>

        <Box className={cx(classes.navGroup)}>
          <SwitchWithGlider
            elements={[
              <Box
                className={cx(classes.navItem, { [classes.active]: nav === 0 })}
                onClick={() => setNav(0)}
              >
                DEX
              </Box>,
              <Box
                className={cx(classes.navItem, { [classes.active]: nav === 1 })}
                onClick={() => setNav(1)}
              >
                SWAP
              </Box>,
              <Box
                className={cx(classes.navItem, { [classes.active]: nav === 2 })}
                onClick={() => setNav(2)}
              >
                MY DASHBOARD
              </Box>,
            ]}
            defaultIndex={1}
            marginBetweenSwitches={2}
          />
        </Box>

        <Box className={cx(classes.statGroup)}>
          <Box className={cx(classes.statBox)}>
            $7.55
            <br />
            <span>$DANA Price</span>
          </Box>
          <Box className={cx(classes.statBox)}>
            $2.65b
            <br />
            <span>Total Liquidity</span>
          </Box>
          <Box className={cx(classes.statBox)}>
            $92.68b
            <br />
            <span>Total Volume</span>
          </Box>
        </Box>
      </Container>
    </Box>
  );
};

export default Launch;
