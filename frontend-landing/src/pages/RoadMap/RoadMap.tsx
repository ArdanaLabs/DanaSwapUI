import React, { useRef, useEffect } from "react"
import { Box } from "@material-ui/core"

import { HeroSection, MapSection } from "./sections"

const RoadMap: React.FC = () => {
  const carouselRef = useRef<any>(null)

  const handleCarousel = (direction: number) => {
    if (direction > 0) {
      carouselRef.current.slideNext()
    } else {
      carouselRef.current.slidePrev()
    }
  }
  useEffect(() => {
    window.scrollTo(0, 0)
  }, [])

  return (
    <Box>
      <HeroSection handleCarousel={handleCarousel} />
      <MapSection carouselRef={carouselRef} />
    </Box>
  )
}

export default RoadMap
