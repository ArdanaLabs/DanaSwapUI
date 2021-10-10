import React from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";
import { Members, TopNotchTeams } from "data";
import ProfileBox, { ProfileType } from "components/Box/ProfileBox";

// import BG_BLUE_RADIAL from "assets/backgrounds/dark-blue-radial-gradient.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: `#080E42`,
    padding: "100px 0",

    [breakpoints.down("sm")]: {
      padding: "50px 0",
    },
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "80px",
    lineHeight: "100%",
    textAlign: "center",
    color: "#F5FCFE",
    whiteSpace: "pre-line",

    "& > span": {
      color: "#73D6F1",
    },

    [breakpoints.down("sm")]: {
      fontSize: "48px",
    },
  },

  content: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "25px",
    lineHeight: "30px",
    textAlign: "center",
    color: "#F5FCFE",
    whiteSpace: "pre-line",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
    },
  },
  alignStretch: {
    display: "flex",
    alignItems: "stretch",
    flexFlow: "column",
  },
}));

const TopNotchSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box
            className={cx(classes.title)}
            dangerouslySetInnerHTML={{
              __html: i18next.t("PAGE.LANDING.TOP-NOTCH.TITLE", {
                interpolation: { escapeValue: false },
              }),
            }}
          />
        </ScrollAnimation>

        <Box mt="30px" />

        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box className={cx(classes.content)}>
            {i18next.t("PAGE.LANDING.TOP-NOTCH.CONTENT")}
          </Box>
        </ScrollAnimation>

        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box
            display="flex"
            flexWrap="wrap"
            alignItems="center"
            justifyContent="center"
            mt="30px"
            style={{ opacity: 0.8 }}
          >
            {TopNotchTeams.map((team, index) => (
              <Box key={index} textAlign="center" px="10px">
                <img
                  src={team}
                  alt="team"
                  height="45px"
                  style={{ maxWidth: "max-content" }}
                />
              </Box>
            ))}
          </Box>
        </ScrollAnimation>

        <Box mt="50px" />

        <Grid container spacing={5}>
          {Members.map((profile: ProfileType, index) => (
            <Grid
              item
              key={index}
              xs={6}
              sm={4}
              md={3}
              className={cx(classes.alignStretch)}
            >
              <ProfileBox profile={profile} />
            </Grid>
          ))}
        </Grid>
      </Container>
    </Box>
  );
};

export default TopNotchSection;
