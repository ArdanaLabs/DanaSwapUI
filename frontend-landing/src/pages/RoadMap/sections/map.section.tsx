import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import Carousel from "react-elastic-carousel"

import { useIsDarkMode } from "state/user/hooks"
import { Roadmap } from "data"
import RoadmapBox from "components/Box/RoadmapBox"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    "minHeight": "100vh",

    "& .rec": {
      pointerEvents: "none",
    },

    "& .rec-carousel-item:not(:last-child) .title:after": {
      // position: "absolute",
      // content: "''",
      // height: "3px",
      // background: "linear-gradient(89.62deg, #72D2F2 0.3%, #6077FF 99.64%)",
      // width: "100px",
      // top: "50%",
      // transform: "translate(0%, -50%)",
      // marginLeft: "-2px",
      // zIndex: 1000,
    },
  },
}))

interface Props {
  carouselRef: any
}

const MapSection: React.FC<Props> = ({ carouselRef }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const renderArrow = () => <></>
  const renderPagination = () => <></>

  return (
    <Box className={cx(classes.root)}>
      <Carousel
        ref={(ref) => (carouselRef.current = ref)}
        // className={cx(classes.carousel)}
        itemsToShow={!mobile ? 4 : 2}
        isRTL={false}
        renderArrow={renderArrow}
        renderPagination={renderPagination}
      >
        {Object.entries(Roadmap).map(([key, value]) => (
          <RoadmapBox term={key} plans={value} key={key} />
        ))}
      </Carousel>
    </Box>
  )
}

export default MapSection
