import { withStyles } from "@material-ui/core/styles"
import { Dialog } from "@material-ui/core"

const StyledDialog = withStyles((theme) => ({
  root: {},
  paperScrollPaper: {
    background:
      theme.palette.type === "light"
        ? "#F6F6F6"
        : theme.palette.background.paper,
    borderRadius: "10px",
    padding: "25px",
  },
}))(Dialog)

export default StyledDialog
