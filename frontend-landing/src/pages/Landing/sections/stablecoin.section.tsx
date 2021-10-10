import React from "react";
import { Box, useMediaQuery, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";
import { GradientButton } from "components/Button";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: "rgba(24, 34, 113, 0.6)",
    padding: "150px 0px",
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "80px",
    lineHeight: "100%",
    color: "#73D6F1",

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
    color: "#FFFFFF",
    width: "50%",
    marginTop: "30px",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
    },
  },
}));

const StableCoinSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <Box mt={!mobile ? "50px" : "0px"}></Box>
        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box className={cx(classes.title)}>
            {i18next.t("PAGE.LANDING.STABLECOIN.TITLE")}
          </Box>
        </ScrollAnimation>
        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box
            className={cx(classes.content)}
            dangerouslySetInnerHTML={{
              __html: i18next.t("PAGE.LANDING.STABLECOIN.CONTENT", {
                interpolation: { escapeValue: false },
              }),
            }}
          />
        </ScrollAnimation>
        <Box mt="50px" />
        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <GradientButton
            label={i18next.t("PAGE.LANDING.STABLECOIN.BUTTON")}
            width={200}
            height={40}
          />
        </ScrollAnimation>
      </Container>
    </Box>
  );
};

export default StableCoinSection;
