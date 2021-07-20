import React, { useState } from "react";
import { Box, Container, Grid, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import cx from "classnames";
import IMG_logo from "assets/logos/Ardana_hor_white.png";
import IMG_bg from "assets/backgrounds/launch-bg.png";
import { useHistory } from "react-router-dom";
import ReactPlayer from "react-player";
const heroVideo =
  "https://background.sfo3.digitaloceanspaces.com/background/output.m3u8";
// "https://background.sfo3.digitaloceanspaces.com/background.mov";

const useStyles = makeStyles(({ palette }) => ({
  root: {
    background: `url(${IMG_bg})`,
    minHeight: "100vh",
    position: "relative",
    height: "100vh",
    "& video": {
      objectFit: "cover",
    },
  },
  container: {
    position: "absolute",
    top: 0,
    width: "100%",
    height: "100%",
    background: `linear-gradient(179.02deg, #000633 26.91%, rgba(0, 5, 38, 0.5) 99.11%)`,
    mixBlendMode: "normal",
    display: "flex",
    justifyContent: "center",
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

    "& > div": {
      display: "flex",
      justifyContent: "center",
    },
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

    "& > div": {
      display: "flex",
      justifyContent: "center",
    },
  },

  statBox: {
    width: "200px",
    // margin: "20px",
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
      <ReactPlayer
        url={heroVideo}
        playing
        loop={true}
        muted
        width="100%"
        height="100%"
      />
      <Box className={cx(classes.container)}>
        <Container>
          <Box className={cx(classes.header)} onClick={() => history.push("/")}>
            <img src={IMG_logo} alt="Ardana Logo" />
          </Box>

          <Box className={cx(classes.title)}>
            Access to your resources,
            <br />
            all in one place.
          </Box>

          <Grid container spacing={1} className={cx(classes.navGroup)}>
            <Grid item xs={12} sm={4}>
              <Box
                className={cx(classes.navItem, {
                  [classes.active]: nav === 0,
                })}
                onClick={() => setNav(0)}
              >
                DEX
              </Box>
            </Grid>
            <Grid item xs={12} sm={4}>
              <Box
                className={cx(classes.navItem, {
                  [classes.active]: nav === 1,
                })}
                onClick={() => setNav(1)}
              >
                SWAP
              </Box>
            </Grid>
            <Grid item xs={12} sm={4}>
              <Box
                className={cx(classes.navItem, {
                  [classes.active]: nav === 2,
                })}
                onClick={() => setNav(2)}
              >
                MY DASHBOARD
              </Box>
            </Grid>
          </Grid>

          <Grid container spacing={1} className={cx(classes.statGroup)}>
            <Grid item xs={12} sm={4}>
              <Box className={cx(classes.statBox)}>
                $7.55
                <br />
                <span>$DANA Price</span>
              </Box>
            </Grid>
            <Grid item xs={12} sm={4}>
              <Box className={cx(classes.statBox)}>
                $2.65b
                <br />
                <span>Total Liquidity</span>
              </Box>
            </Grid>
            <Grid item xs={12} sm={4}>
              <Box className={cx(classes.statBox)}>
                $92.68b
                <br />
                <span>Total Volume</span>
              </Box>
            </Grid>
          </Grid>
        </Container>
      </Box>
    </Box>
  );
};

export default Launch;
