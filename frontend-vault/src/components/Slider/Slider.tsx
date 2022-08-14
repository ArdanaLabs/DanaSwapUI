import { Slider, Theme } from "@mui/material"
import { withStyles } from "@mui/styles"
import { FontFamilies } from "theme"

const StyledSlider = withStyles((theme: Theme) => ({
  root: {},
  track: {
    height: 6,
    borderRadius: 2,
    color: "transparent",
  },
  thumb: {
    height: 24,
    width: 24,
    background: theme.palette.info.light,
  },
  rail: {
    height: "4px",
    backgroundColor: `${theme.palette.info.dark}!important`,
    opacity: "1 !important",
    borderRadius: 10,
  },
  mark: {
    background: "unset",
  },
  markLabel: {
    paddingTop: "5px",
    color: theme.palette.primary.main,
    fontFamily: FontFamilies.Museo,
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "14px",
    lineHeight: "100%",
  },
}))(Slider)

export default StyledSlider
