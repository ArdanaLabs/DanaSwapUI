import { withStyles } from "@material-ui/core/styles";
import Button from "@material-ui/core/Button";

const StyledButton = withStyles({
  root: {
    position: "relative",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    padding: "10px 30px",
    lineHeight: '100%',
    borderRadius: "50px",
    fontSize: "13px",
    fontWeight: 700,
    backgroundColor: "#080E42",
    color: "#F5FCFE",
    whiteSpace: "nowrap",
    boxShadow: '0px 0px 14px 0px #2d3ba0',
    
    "&:hover": {
      backgroundColor: "lightgray",
    },

    "&::after": {
      position: "absolute",
      top: "-3px",
      bottom: "-3.5px",
      left: "-3px",
      right: "-3.5px",
      background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
      content: "' '",
      zIndex: -1,
      borderRadius: "50px",
    },
  },
})(Button);

export default StyledButton;
