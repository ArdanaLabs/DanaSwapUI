import { Slider } from "@material-ui/core"
import { withStyles } from "@material-ui/core/styles"

const StyledSlider = withStyles((theme) => ({
  root: {
    maxWidth: "270px",
  },
  track: {
    height: 3,
    borderRadius: 2,
    color: "transparent",
  },
  thumb: {
    height: 16,
    width: 16,
    marginTop: 1.5,
    marginLeft: 0,
    color: theme.palette.secondary.main,
    transform: "translate(-50%, -50%)",
  },
  rail: {
    height: 3,
    background: `linear-gradient(-90deg, ${theme.palette.secondary.dark} 16.67%, ${theme.palette.secondary.main} 99.98%, ${theme.palette.secondary.dark} 99.99%)`,
    opacity: 1,
    borderRadius: 10,
  },
  mark: {
    background: "unset",
  },
  markLabel: {
    paddingTop: "5px",
    color: theme.palette.primary.main,
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "16px",
    lineHeight: "100%",
    [`&[data-index="0"]`]: {
      transform: "translateX(0%)",
    },
    [`&[data-index="1"]`]: {
      transform: "translateX(-100%)",
    },
  },
}))(Slider)

export default StyledSlider
