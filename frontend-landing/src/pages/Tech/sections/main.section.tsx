import React, { useRef } from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import Carousel from "react-elastic-carousel";

import { useIsDarkMode } from "state/user/hooks";
import { GradientButton } from "components/Button";

import BG_PURPLE_RADIAL from "assets/backgrounds/pink-gradient.png";
import BG_BLUE_RADIAL from "assets/backgrounds/cyan-gradient.png";
import BG_CIRCLE from "assets/backgrounds/two-circle.png";
import ICO_NEXT from "assets/icons/carousel-next.svg";
import ICO_PREV from "assets/icons/carousel-prev.svg";
import { TechnicalPapers } from "data";
import { TechnicalPaperBox } from "components";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: ` url(${BG_PURPLE_RADIAL}) top -700px right -700px no-repeat,
                  url(${BG_BLUE_RADIAL}) top -700px left -1000px no-repeat,
                  url(${BG_CIRCLE}) top 100px left no-repeat`,
    paddingTop: "300px",
    backgroundSize: "auto, auto, auto calc(100% - 200px)",

    [breakpoints.down("xs")]: {
      background: ` url(${BG_BLUE_RADIAL}) top -700px left -1000px no-repeat,
                    url(${BG_CIRCLE}) top 100px left -70px no-repeat`,
      backgroundSize: "auto, auto 300px",
      textAlign: "center",
      paddingTop: "180px",
    },
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "60px",
    lineHeight: "100%",
    color: palette.text.secondary,

    [breakpoints.down("xs")]: {
      fontSize: "35px",
    },
  },

  content: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "16px",
    lineHeight: "25px",
    color: palette.text.primary,
    marginTop: "30px",

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
      marginTop: "15px",
      padding: "0 50px 100px",
    },
  },

  carousel: {},
  carouselAction: {
    display: "flex",
    justifyContent: "right",
    paddingRight: "20px",
    paddingTop: "20px",
    [breakpoints.down("xs")]: {
      justifyContent: "center",
    },
  },

  image: {
    position: "relative",
    lineHeight: 0,
    margin: "20px 10px",
  },

  photo: {
    position: "absolute",
    top: "50%",
    left: "50%",
    transform: "translate(-50%, -50%)",
    display: "inline-flex",
    justifyContent: "center",
    alignItems: "center",
    borderRadius: "50%",
    width: "25px",
  },
}));

const MainSection: React.FC = () => {
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
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container spacing={0} alignItems="flex-start">
          <Grid item xs={12} sm={3} md={4}>
            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box className={cx(classes.title)}>Explore</Box>
            </ScrollAnimation>
            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box className={cx(classes.content)}>
                Browse through our technical papers to learn more about the
                Ardana ecosystem.
              </Box>
            </ScrollAnimation>
          </Grid>

          <Grid item xs={12} sm={9} md={8}>
            <Carousel
              ref={(ref) => (carouselRef.current = ref)}
              className={cx(classes.carousel)}
              itemsToShow={!mobile ? 2 : 1}
              isRTL={false}
              itemPadding={!mobile ? [0, 20] : [0, 10]}
              renderArrow={renderArrow}
              renderPagination={renderPagination}
            >
              {TechnicalPapers.map((paper, i: number) => (
                <TechnicalPaperBox key={paper.title} {...paper} />
              ))}
              {/* {DanaSwapFeatures.map((feature, i: number) => (
                <DanaswapFeature
                  key={feature.title + i}
                  image={feature.image}
                  title={feature.title}
                  content={feature.content}
                />
              ))} */}
            </Carousel>
            <Box className={cx(classes.carouselAction)}>
              <Box
                className={cx(classes.image)}
                onClick={() => handleCarousel(-1)}
              >
                <GradientButton
                  width={!mobile ? 75 : 50}
                  height={!mobile ? 75 : 50}
                />
                <img className={cx(classes.photo)} src={ICO_PREV} alt="prev" />
              </Box>
              <Box
                className={cx(classes.image)}
                onClick={() => handleCarousel(1)}
              >
                <GradientButton
                  width={!mobile ? 75 : 50}
                  height={!mobile ? 75 : 50}
                />
                <img className={cx(classes.photo)} src={ICO_NEXT} alt="next" />
              </Box>
            </Box>
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default MainSection;
