import React from "react";
import { Box, Container, Grid, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import cx from "classnames";
import IMG_bg from "assets/backgrounds/launch-bg.png";
import ReactPlayer from "react-player";
import ScrollAnimation from "react-animate-on-scroll";

const heroVideo =
  "https://background.sfo3.digitaloceanspaces.com/background/output.m3u8";
// "https://background.sfo3.digitaloceanspaces.com/background.mov";

const statInfo = [
  {
    label: "Market Cap",
    content: "$7 million",
  },
  {
    label: "$DANA Price",
    content: "$7.00",
  },
  {
    label: "Ecosystem TVL",
    content: "$92.68 billion",
  },
  {
    label: "% of DANA\nTokens Locked",
    content: "92%",
  },
  {
    label: "DANA Tokens\nHolders",
    content: "19,837",
  },
  {
    label: "Current exDANA\nStaking APY",
    content: "88,000",
  },
];

const useStyles = makeStyles(({ palette }) => ({
  root: {
    background: `url(${IMG_bg})`,
    minHeight: "100vh",
    position: "fixed",
    height: "100vh",
    width: "100vw",
    "& video": {
      objectFit: "cover",
    },
  },

  container: {
    position: "absolute",
    top: 0,
    left: 0,
    width: "100vw",
    height: "100vh",
    background: `linear-gradient(180deg, #01062F 14.3%, rgba(0, 6, 51, 0.5) 70.55%)`,
    mixBlendMode: "normal",
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "48px",
    lineHeight: "120.5%",
    textAlign: "center",
    color: "#FFFFFF",
    padding: "30px",

    "& > span": {
      fontWeight: 900,
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

  StatBox: {
    color: "white",
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "22px",
    lineHeight: "110%",
    whiteSpace: "pre-line",

    "& > span": {
      fontFamily: "Museo Sans",
      fontSize: "12px",
      lineHeight: "16px",
      fontWeight: 100,
    },
  },
}));

export interface TotalStatsSectionProps {
  top?: string;
  show?: boolean;
}

const TotalStatsSection: React.FC<TotalStatsSectionProps> = ({
  top = "0vh",
  show = false,
}) => {
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const dark = useIsDarkMode();
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)} top={top}>
      <ReactPlayer
        url={heroVideo}
        playing
        loop={true}
        muted
        width="100%"
        height="100%"
        playbackRate={0.5}
      />
      <Box className={cx(classes.container)}>
        {show && (
          <Container>
            <ScrollAnimation animateIn="fadeInUp">
              <Box className={cx(classes.title)}>
                DECENTRALIZED
                <br />
                <span>STABLECOIN HUB</span>
              </Box>
            </ScrollAnimation>

            <Grid container spacing={1} className={cx(classes.statGroup)}>
              {statInfo.map((stat: any, i: number) => (
                <Grid item xs={6} sm={4} md={2} key={i}>
                  <ScrollAnimation animateIn="flipInY" animateOut="flipOutY">
                    <Box className={cx(classes.StatBox)}>
                      {stat.content}
                      <br />
                      <span>{stat.label}</span>
                    </Box>
                  </ScrollAnimation>
                </Grid>
              ))}
            </Grid>
          </Container>
        )}
      </Box>
    </Box>
  );
};

export default TotalStatsSection;
