import { withStyles } from "@material-ui/core/styles"
import Button from "@material-ui/core/Button"

const StyledButton = withStyles({
  root: {
    padding: "4px 43px 7px 43px",
    borderRadius: "25px",
    // backgroundColor: "#A5A5A5",
    color: "#FFFFFF",
    whiteSpace: "nowrap",
    fontFamily: "Museo Sans, sans-serif",
    fontStyle: "normal",
    fontWeight: "bold",
    fontSize: "13px",
    lineHeight: "18px",
  },
})(Button)

export default StyledButton
