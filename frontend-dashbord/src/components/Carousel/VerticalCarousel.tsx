import React from "react";
import cx from "classnames";
import { makeStyles, useMediaQuery, useTheme } from "@material-ui/core";
import { useIsDarkMode } from "state/user/hooks";

/*
 * Read the blog post here:
 * https://letsbuildui.dev/articles/building-a-vertical-carousel-component-in-react
 */

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  carouselInner: {
    position: "relative",
    maxHeight: "300px",
    height: "300px",
  },

  carouselItem: {
    position: "absolute",
    background: "none",
    border: "none",
    padding: 0,
    margin: 0,
    opacity: 1,
    top: "112px",
    transition: "transform 0.4s ease, opacity 0.4s ease",
    fontFamily: "'Brandon Grotesque'",
    fontStyle: "normal",
    fontWeight: 100,
    fontSize: "32px",
    color: "white",
    textAlign: "left",
    cursor: "pointer",
  },

  visible: {
    opacity: 1,
  },

  active: {
    fontSize: "45px",
    textAlign: "left",
    fontWeight: 900,
    color: "white",
    whiteSpace: "pre-line",
  },

  before: {
    background: "-webkit-linear-gradient(-90deg, #ffffff, #ffffff00)",
    "-webkit-background-clip": "text",
    "-webkit-text-fill-color": "transparent",
  },
  after: {
    background: "-webkit-linear-gradient(90deg, #ffffff, #ffffff00)",
    "-webkit-background-clip": "text",
    "-webkit-text-fill-color": "transparent",
  },
}));

export interface VerticalCarouselProps {
  data: any[];
  activeIndex: number;
  setActiveIndex: any;
}

const VerticalCarousel: React.FC<VerticalCarouselProps> = ({
  data,
  activeIndex,
  setActiveIndex,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  // const [activeIndex, setActiveIndex] = useState(0);

  // Used to determine which items appear above the active item
  const halfwayIndex = Math.ceil(data.length / 2);

  // Usd to determine the height/spacing of each item
  const itemHeight = 120;

  // Used to determine at what point an item is moved from the top to the bottom
  const shuffleThreshold = halfwayIndex * itemHeight;

  // Used to determine which items should be visible. this prevents the "ghosting" animation
  const visibleStyleThreshold = shuffleThreshold / 2;

  const determinePlacement = (itemIndex: number): any => {
    // If these match, the item is active
    if (activeIndex === itemIndex) {
      if (itemIndex !== 1) return -15;
      return -45;
    }

    if (itemIndex >= halfwayIndex) {
      if (activeIndex > itemIndex - halfwayIndex) {
        return (itemIndex - activeIndex) * itemHeight;
      } else {
        return -(data.length + activeIndex - itemIndex) * itemHeight;
      }
    }

    if (itemIndex > activeIndex) {
      return (itemIndex - activeIndex) * itemHeight;
    }

    if (itemIndex < activeIndex) {
      if ((activeIndex - itemIndex) * itemHeight >= shuffleThreshold) {
        return (data.length - (activeIndex - itemIndex)) * itemHeight;
      }
      return -(activeIndex - itemIndex) * itemHeight;
    }
  };

  return (
    <div className={cx(classes.carouselInner)}>
      {data.map((item: any, i: number) => (
        <button
          type="button"
          onClick={() => setActiveIndex(i)}
          className={cx(classes.carouselItem, {
            [classes.active]: activeIndex === i,
            [classes.visible]:
              Math.abs(determinePlacement(i)) <= visibleStyleThreshold,
            [classes.before]: determinePlacement(i) === -1 * itemHeight,
            [classes.after]: determinePlacement(i) === 1 * itemHeight,
          })}
          key={i}
          style={{
            transform: `translateY(${determinePlacement(i)}px)`,
          }}
        >
          {item}
        </button>
      ))}
    </div>
  );
};

export default VerticalCarousel;
