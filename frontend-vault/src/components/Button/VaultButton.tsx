import { Theme, Button } from "@mui/material"
import { withStyles } from "@mui/styles"

const StyledVaultButton = withStyles((theme: Theme) => ({
  root: {
    background: theme.palette.info.light,
    borderRadius: "100px !important",
    padding: "10px 25px !important",
    minWidth: "150px !important",
  },
}))(Button)

export default StyledVaultButton
