import React from "react";
import { Box, useMediaQuery, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";

import i18next from "i18next";
import { useIsDarkMode } from "state/user/hooks";

import { Investors } from "data";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: palette.background.default,
    padding: "50px 20px",

    [breakpoints.down("xs")]: {
      padding: "20px 0",
    },
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontWeight: 900,
    fontSize: "80px",
    lineHeight: "100%",
    color: palette.text.secondary,
    textAlign: "center",

    [breakpoints.down("xs")]: {
      fontSize: "35px",
    },
  },
  partner: {
    textAlign: "center",

    "& img": {
      width: "100%",
      maxWidth: "max-content",
    },
  },
}));

const PartnerSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box className={cx(classes.title)}>
            {i18next.t("PAGE.LANDING.INVESTORS")}
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
            {Investors.map((investor, index) => (
              <Box key={index} textAlign="center" p={!mobile ? "20px" : "10px"}>
                <img
                  src={investor}
                  alt="investor"
                  height={!mobile ? "45px" : "25px"}
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

export default PartnerSection;