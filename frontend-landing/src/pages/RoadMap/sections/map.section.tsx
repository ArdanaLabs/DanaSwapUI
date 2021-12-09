import React, { useRef } from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import Carousel from "react-elastic-carousel";

import { useIsDarkMode } from "state/user/hooks";
import { Roadmap } from "data";
import RoadmapBox from "components/Box/RoadmapBox";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    minHeight: "100vh",
  },
}));

const MapSection: React.FC = () => {
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
      <Carousel
        ref={(ref) => (carouselRef.current = ref)}
        // className={cx(classes.carousel)}
        itemsToShow={!mobile ? 4 : 2}
        isRTL={false}
        renderArrow={renderArrow}
        renderPagination={renderPagination}
      >
        {Object.entries(Roadmap).map(([key, value]) => (
          <RoadmapBox term={key} plans={value} />
        ))}
      </Carousel>
    </Box>
  );
};

export default MapSection;
