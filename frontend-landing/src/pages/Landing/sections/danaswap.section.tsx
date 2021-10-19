import React, { useRef } from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import Carousel from "react-elastic-carousel";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";
import { GradientButton } from "components/Button";
import { DanaswapFeature } from "components/Box";

import ICO_NEXT from "assets/icons/carousel-next.svg";
import ICO_PREV from "assets/icons/carousel-prev.svg";

const Ardana_features = [
  {
    image: require("assets/logos/ultra-low-slippage.svg").default,
    title: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.0.TITLE"),
    content: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.0.CONTENT"),
  },
  {
    image: require("assets/logos/saving-account.svg").default,
    title: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.1.TITLE"),
    content: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.1.CONTENT"),
  },
  {
    image: require("assets/logos/saving-account.svg").default,
    title: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.2.TITLE"),
    content: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.2.CONTENT"),
  },
  {
    image: require("assets/logos/saving-account.svg").default,
    title: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.3.TITLE"),
    content: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.3.CONTENT"),
  },
];

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: "#080E42",
    padding: "100px 0px",

    [breakpoints.down("xs")]: {
      padding: "40px 0px",
      textAlign: "center",
    },
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "70px",
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
    fontSize: "22px",
    lineHeight: "26px",
    color: "#F5FCFE",

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
    },
  },

  carousel: {
    borderRadius: "10px",
    background: palette.background.paper,

    [breakpoints.down("xs")]: {
      background: "transparent",
      marginLeft: "-16px",
      width: "100vw !important",
      "& .rec .rec-slider-container": {
        margin: "0 !important",
      },
    },
  },
  carouselAction: {
    display: "flex",
    justifyContent: "left",
    "& img": {
      cursor: "pointer",
      width: "100px",
    },
    [breakpoints.down("xs")]: {
      justifyContent: "center",
    },
  },
}));

const DanaswapSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  const carouselRef = useRef<any>(null);

  const renderArrow = () => <></>;
  const renderPagination = () => <></>;

  const handleCarousel = (direction: number) => {
    if (direction > 0) {
      carouselRef.current.slideNext();
    } else {
      carouselRef.current.slidePrev();
    }
  };

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
              ref={(ref) => (carouselRef.current = ref)}
              className={cx(classes.carousel)}
              itemsToShow={!mobile ? 2 : 1}
              isRTL={false}
              renderArrow={renderArrow}
              renderPagination={renderPagination}
            >
              {Ardana_features.map((feature, i: number) => (
                <DanaswapFeature
                  key={feature.title + i}
                  image={feature.image}
                  title={feature.title}
                  content={feature.content}
                />
              ))}
            </Carousel>
            <Box className={cx(classes.carouselAction)}>
              <Box onClick={() => handleCarousel(-1)}>
                <img src={ICO_PREV} alt="prev" />
              </Box>
              <Box onClick={() => handleCarousel(1)}>
                <img src={ICO_NEXT} alt="next" />
              </Box>
            </Box>
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default DanaswapSection;
