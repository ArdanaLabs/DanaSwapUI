import React from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ReactPlayer from "react-player";
import ScrollAnimation from "react-animate-on-scroll";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";
import { GradientButton } from "components/Button";

const heroVideo =
  "https://background.sfo3.digitaloceanspaces.com/team/output.webm";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    padding: "0px",
    position: "relative",
    "& video": {
      objectFit: "cover",
    },
  },
  background: {
    lineHeight: 0,

    "& > img": {
      width: "100%",
      height: "600px",

      [breakpoints.down("xs")]: {
        height: "600px",
      },
    },
  },
  container: {
    position: "absolute",
    top: 0,
    left: 0,
    width: "100%",
    height: "100%",
    display: "flex",
    alignItems: "center",
    // background: "rgba(24, 34, 113, 0.6)",
    background:
      "linear-gradient(180deg, rgba(4, 13, 77, 0.7) -43.4%, rgba(50, 3, 111, 0.7) 222.51%)",

    [breakpoints.down("xs")]: {
      textAlign: "center",
    },
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "60px",
    lineHeight: "100%",
    color: "#73D6F1",

    [breakpoints.down("sm")]: {
      fontSize: "35px",
    },
  },

  content: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "16px",
    lineHeight: "26px",
    width: "100%",
    color: palette.text.primary,
    marginTop: "30px",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
      width: "100%",
      marginTop: "15px",
      padding: "0px 10px",
    },
  },
}));

const RoadMapSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <ReactPlayer
        url={heroVideo}
        playing={true}
        loop={true}
        muted
        width={!mobile ? "100%" : "unset"}
        height={!mobile ? "600px" : "400px"}
        playbackRate={0.3}
      />
      <Box className={cx(classes.container)}>
        <Container>
          <Grid container spacing={2} alignItems="center">
            <Grid item xs={12} sm={6}>
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box className={cx(classes.title)}>
                  {i18next.t("PAGE.LANDING.ROADMAP.TITLE")}
                </Box>
              </ScrollAnimation>
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box
                  className={cx(classes.content)}
                  dangerouslySetInnerHTML={{
                    __html: i18next.t("PAGE.LANDING.ROADMAP.CONTENT", {
                      interpolation: { escapeValue: false },
                    }),
                  }}
                />
              </ScrollAnimation>
              <Box mt={!mobile ? "50px" : "30px"} />
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <GradientButton
                  label={"VIEW ROADMAP"}
                  width={160}
                  height={40}
                />
              </ScrollAnimation>
            </Grid>
          </Grid>
        </Container>
      </Box>
    </Box>
  );
};

export default RoadMapSection;
