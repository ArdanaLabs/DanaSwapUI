import { withStyles } from "@material-ui/core/styles";
import Button from "@material-ui/core/Button";

const StyledButton = withStyles({
  root: {
    margin: "10px",
    padding: "15px 30px",
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
