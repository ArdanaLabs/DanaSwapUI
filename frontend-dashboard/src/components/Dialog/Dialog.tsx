import { withStyles } from "@material-ui/core/styles"
import { Dialog } from "@material-ui/core"

const StyledDialog = withStyles((theme) => ({
  root: {},
  paperScrollPaper: {
    background: `linear-gradient(126.33deg, ${theme.palette.background.paper} 9.83%, #00000000 96.44%), ${theme.palette.background.default}`,
    borderRadius: "10px",
    padding: "25px",
  },
}))(Dialog)

export default StyledDialog
