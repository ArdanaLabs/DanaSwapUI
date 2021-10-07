import { Select } from "@material-ui/core";
import { withStyles } from "@material-ui/core/styles";

const StyledSelect = withStyles({
  root: {
    width: "100px",
    borderRadius: "50px",
    border: "unset",
  },
})(Select);

export default StyledSelect;
