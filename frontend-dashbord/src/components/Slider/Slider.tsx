import { Slider } from "@material-ui/core";
import { withStyles } from "@material-ui/core/styles";

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
    marginTop: -9,
    marginLeft: -10,
    color: "#73D6F1",
  },
  rail: {
    height: "6px",
    background:
      "linear-gradient(-90deg, #2F3DA0 16.67%, #73D6F1 99.98%, #5F72FF 99.99%)",
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
}))(Slider);

export default StyledSlider;
