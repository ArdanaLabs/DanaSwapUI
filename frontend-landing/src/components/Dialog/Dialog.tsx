import { withStyles } from "@material-ui/core/styles";
import { Dialog } from "@material-ui/core";

const StyledDialog = withStyles((theme) => ({
  root: {

  },
  paperScrollPaper: {
    background: theme.palette.type === "light" ? "#F6F6F6" : "linear-gradient(180deg, rgba(47, 61, 160, 0) 0%, #2F3DA0 58.33%)",
    borderRadius: "10px",
    padding: "25px",
  }
}))(Dialog);

export default StyledDialog;
