import React, { useRef } from "react"
import {
  Box,
  useMediaQuery,
  Typography,
  Container,
  Grid,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import Carousel from "react-elastic-carousel"

import { useIsDarkMode } from "state/user/hooks"
import { GradientButton } from "components/Button"

import BG_CIRCLE from "assets/backgrounds/two-circle-rtl.png"
import ICO_NEXT from "assets/icons/carousel-next.svg"
import ICO_PREV from "assets/icons/carousel-prev.svg"
import { TechnicalPapers } from "data"
import { TechnicalPaperBox } from "components"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: `url(${BG_CIRCLE}) top right no-repeat`,
    paddingTop: "200px",
    backgroundSize: "auto 100%",

    [breakpoints.down("xs")]: {
      background: "none !important",
      textAlign: "center",
      paddingTop: "100px",
    },
  },

  title: {
    color: palette.secondary.main,
    lineHeight: "100%",
  },

  content: {
    lineHeight: "25px",
    marginTop: "30px",

    [breakpoints.down("xs")]: {
      lineHeight: "18.4px",
      marginTop: "15px",
      padding: "0 50px 50px",
    },
  },

  carouselAction: {
    display: "flex",
    justifyContent: "right",
    paddingRight: "20px",
    paddingTop: "20px",
    [breakpoints.down("xs")]: {
      justifyContent: "center",
      padding: 0,
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

const TechSection: React.FC = () => {
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
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container spacing={0} alignItems="flex-start">
          <Grid item xs={12} sm={3} md={4}>
            <Typography
              variant="h3"
              component="h3"
              className={cx(classes.title)}
            >
              Explore
            </Typography>
            <Typography
              variant="h4"
              component="h4"
              className={cx(classes.content)}
            >
              Browse through our technical papers to learn more about the Ardana
              ecosystem.
            </Typography>
          </Grid>

          <Grid item xs={12} sm={9} md={8}>
            <Carousel
              ref={(ref) => (carouselRef.current = ref)}
              itemsToShow={!mobile ? 2 : 1}
              isRTL={false}
              itemPadding={!mobile ? [0, 20] : [0, 10]}
              renderArrow={renderArrow}
              renderPagination={renderPagination}
            >
              {TechnicalPapers.map((paper, i: number) => (
                <TechnicalPaperBox key={paper.title} {...paper} />
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

export default TechSection
