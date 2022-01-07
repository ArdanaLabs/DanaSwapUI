import { Slider } from "@material-ui/core"
import { withStyles } from "@material-ui/core/styles"

const StyledSlider = withStyles((theme) => ({
  root: {},
  track: {
    height: 6,
    borderRadius: 2,
    color: "transparent",
  },
  thumb: {
    height: 24,
    width: 24,
    marginTop: -11,
    marginLeft: -10,
    background: theme.palette.info.light,
  },
  rail: {
    height: "4px",
    background: theme.palette.info.dark,
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
    fontSize: "14px",
    lineHeight: "100%",
  },
}))(Slider)

export default StyledSlider
