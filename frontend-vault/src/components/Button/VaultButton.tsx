import { Button } from "@material-ui/core"
import { withStyles, Theme } from "@material-ui/core/styles"

const StyledValutButton = withStyles((theme: Theme) => ({
  root: {
    background: theme.palette.info.light,
    borderRadius: "100px",
    fontWeight: 700,
    fontSize: "15px",
    color: theme.palette.common.white,
    padding: "10px 25px",
    minWidth: "150px",
    lineHeight: "100% !important",
    textTransform: "uppercase",
  },
}))(Button)

export default StyledValutButton
