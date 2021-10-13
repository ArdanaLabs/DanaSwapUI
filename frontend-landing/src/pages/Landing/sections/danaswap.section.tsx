import React from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import Carousel from "react-elastic-carousel";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";
import { GradientButton } from "components/Button";
import { DanaswapFeature } from "components/Box";

const Ardana_features = [
  {
    image: require("assets/logos/fully-decentralized.svg").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.0.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.0.CONTENT"),
  },
  {
    image: require("assets/logos/borrow-lend.svg").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.1.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.1.CONTENT"),
  },
  {
    image: require("assets/logos/store-of-value.svg").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.2.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.2.CONTENT"),
  },
  {
    image: require("assets/logos/powered-by-cardano.svg").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.3.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.3.CONTENT"),
  },
];

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: "#080E42",
    padding: "100px 0px",

    [breakpoints.down("xs")]: {
      padding: "40px 10px",
      textAlign: "center",
    },
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "80px",
    lineHeight: "100%",
    color: "#73D6F1",

    [breakpoints.down("xs")]: {
      fontSize: "35px",
    },
  },

  content: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "25px",
    lineHeight: "30px",
    color: "#F5FCFE",

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
    },
  },

  carousel: {
    borderRadius: "10px",
    background: palette.background.paper,
  },
}));

const DanaswapSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const renderArrow = () => <></>;
  const renderPagination = () => <></>;

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <Grid container alignItems="center" spacing={3}>
          <Grid item xs={12} sm={6}>
            <Box>
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box className={cx(classes.title)}>
                  {i18next.t("PAGE.LANDING.DANASWAP.TITLE")}
                </Box>
              </ScrollAnimation>
              <Box mt="20px" />
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box className={cx(classes.content)}>
                  {i18next.t("PAGE.LANDING.DANASWAP.CONTENT")}
                </Box>
              </ScrollAnimation>
              <Box mt={!mobile ? "50px" : "30px"} />
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <GradientButton
                  label={i18next.t("PAGE.LANDING.DANASWAP.BUTTON")}
                  width={200}
                  height={40}
                />
              </ScrollAnimation>
            </Box>
          </Grid>

          <Grid item xs={12} sm={6}>
            <Carousel
              className={cx(classes.carousel)}
              itemsToShow={2}
              isRTL={false}
              renderArrow={renderArrow}
              renderPagination={renderPagination}
            >
              <DanaswapFeature
                image={Ardana_features[0].image}
                title={Ardana_features[0].title}
                content={Ardana_features[0].content}
              />
              <DanaswapFeature
                image={Ardana_features[0].image}
                title={Ardana_features[0].title}
                content={Ardana_features[0].content}
              />
              <DanaswapFeature
                image={Ardana_features[0].image}
                title={Ardana_features[0].title}
                content={Ardana_features[0].content}
              />
            </Carousel>
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default DanaswapSection;
