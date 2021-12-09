import React, { useRef } from "react";
import { Box } from "@material-ui/core";

import { HeroSection, MapSection } from "./sections";

const RoadMap: React.FC = () => {
  const carouselRef = useRef<any>(null);

  const handleCarousel = (direction: number) => {
    if (direction > 0) {
      carouselRef.current.slideNext();
    } else {
      carouselRef.current.slidePrev();
    }
  };

  return (
    <Box>
      <HeroSection handleCarousel={handleCarousel} />
      <MapSection carouselRef={carouselRef} />
    </Box>
  );
};

export default RoadMap;
