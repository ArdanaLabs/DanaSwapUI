import { withStyles } from "@material-ui/core/styles";
import Button from "@material-ui/core/Button";

const StyledButton = withStyles({
  root: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    LineHeight: "26px",
    padding: "20px 50px",
    borderRadius: "50px",
    fontSize: "18px",
    fontWeight: 900,
    backgroundColor: "#FFFFFF",
    color: "#202F9A",
    whiteSpace: 'nowrap',
    width: "300px",
    "&:hover": {
      color: "#FFFFFF",
      backgroundColor: "#202F9A",
    },
  },
})(Button);

export default StyledButton;
