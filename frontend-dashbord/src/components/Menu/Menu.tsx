import { withStyles } from "@material-ui/core/styles";
import { Menu } from "@material-ui/core";

const StyledMenu = withStyles((theme) => ({
  paper: {
    color: theme.palette.secondary.main,
    background: theme.palette.background.default,
    borderRadius: "5px",
    border: "unset",
  },
}))(Menu);

export default StyledMenu;
