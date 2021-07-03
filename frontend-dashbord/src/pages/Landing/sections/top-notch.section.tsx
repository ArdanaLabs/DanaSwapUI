import React from "react";
import { Box, useMediaQuery, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import { withTranslation, WithTranslation } from "react-i18next";

import { useIsDarkMode } from "state/user/hooks";

import LOGO_APPLE from "assets/img/landing/logos/logo_apple.svg";
import LOGO_MICROSOFT from "assets/img/landing/logos/logo_microsoft.svg";
import LOGO_BARCLAYS from "assets/img/landing/logos/logo_barclays.svg";
import LOGO_MINA from "assets/img/landing/logos/logo_mina.svg";
import LOGO_RAKUTEN from "assets/img/landing/logos/logo_rakuten.svg";
import LOGO_IG from "assets/img/landing/logos/logo_ig.svg";
import LOGO_CARDANO from "assets/img/landing/logos/logo_cardano.svg";
import LOGO_EMURGO from "assets/img/landing/logos/logo_emurgo.svg";

const TopNotchTeams = [
  {
    logo: LOGO_APPLE,
  },
  {
    logo: LOGO_MICROSOFT,
  },
  {
    logo: LOGO_BARCLAYS,
  },
  {
    logo: LOGO_MINA,
  },
  {
    logo: LOGO_RAKUTEN,
  },
  {
    logo: LOGO_IG,
  },
  {
    logo: LOGO_CARDANO,
  },
  {
    logo: LOGO_EMURGO,
  },
];

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: "#FFFFFF",
    padding: "100px 0",

    [breakpoints.down("sm")]: {
      padding: "50px 0",
    },
  },

  title: {
    fontFamily: "Brandon Grotesque Bold",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "64px",
    lineHeight: "100%",
    textAlign: "center",
    color: "#202F9A",
    padding: "20px",

    [breakpoints.down("sm")]: {
      fontSize: "48px",
    },
  },

  content: {
    fontFamily: "'Museo Sans 300'",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "18px",
    lineHeight: "150%",
    textAlign: "center",
    color: "#202020",
    padding: "20px",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
    },
  },
}));

const TopNotchSection: React.FC<WithTranslation> = ({ t }) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box className={cx(classes.title)}>
            {t("PAGE.LANDING.TOP-NOTCH.TITLE")}
          </Box>
        </ScrollAnimation>

        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box className={cx(classes.content)}>
            {t("PAGE.LANDING.TOP-NOTCH.CONTENT")}
          </Box>
        </ScrollAnimation>

        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box
            display="flex"
            flexWrap="wrap"
            alignItems="center"
            justifyContent="center"
            p="20px"
          >
            {TopNotchTeams.map((team, index) => (
              <Box
                key={index}
                p={!mobile ? "0px 30px" : "10px"}
                width={!mobile ? "auto" : "50%"}
                textAlign="center"
              >
                <img
                  src={team.logo}
                  alt=""
                  width="100%"
                  height="100%"
                  style={{ maxWidth: "max-content" }}
                />
              </Box>
            ))}
          </Box>
        </ScrollAnimation>
      </Container>
    </Box>
  );
};

export default withTranslation()(TopNotchSection);
