import React, { useRef } from "react"
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import ScrollAnimation from "react-animate-on-scroll"
import Carousel from "react-elastic-carousel"
import i18next from "i18next"

import { useIsDarkMode } from "state/user/hooks"
import { GradientButton } from "components/Button"
import { DanaswapFeature } from "components/Box"

import { DanaSwapFeatures } from "data"

import ICO_NEXT from "assets/icons/carousel-next.svg"
import ICO_PREV from "assets/icons/carousel-prev.svg"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    paddingTop: "100px",
    paddingBottom: "20px",

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
    lineHeight: "26px",
    color: palette.text.primary,

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
    },
  },

  carousel: {
    borderRadius: "10px",
    background: palette.background.paper,

    [breakpoints.down("xs")]: {
      "background": "transparent",
      "marginLeft": "-16px",
      "width": "100vw !important",
      "& .rec .rec-slider-container": {
        margin: "0 !important",
      },
    },
  },
  carouselAction: {
    display: "flex",
    justifyContent: "left",
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

    [breakpoints.down("xs")]: {
      width: "18px",
    },
  },
}))

const DanaswapSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const carouselRef = useRef<any>(null)

  const renderArrow = () => <></>
  const renderPagination = () => <></>

  const handleCarousel = (direction: number) => {
    if (direction > 0) {
      carouselRef.current.slideNext()
    } else {
      carouselRef.current.slidePrev()
    }
  }

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
                <GradientButton label={"COMING SOON"} width={160} height={40} />
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
              {DanaSwapFeatures.map((feature, i: number) => (
                <DanaswapFeature
                  key={feature.title + i}
                  image={feature.image}
                  title={feature.title}
                  content={feature.content}
                />
              ))}
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
  )
}

export default DanaswapSection
