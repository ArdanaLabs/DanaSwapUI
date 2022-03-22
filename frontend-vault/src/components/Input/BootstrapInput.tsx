import { Theme, createStyles, InputBase } from "@mui/material"
import { withStyles } from "@mui/styles"
import { FontFamilies } from "theme"

const BootstrapInput = withStyles((theme: Theme) =>
  createStyles({
    root: {
      width: "100%",
      [`& > svg`]: {
        right: "10px",
        color: "white",
      },
    },
    input: {
      fontFamily: FontFamilies.Brandon,
      fontWeight: "900 !important",
      borderRadius: "100px !important",
      color: `${theme.palette.common.white} !important`,
      width: "100%",
      position: "relative",
      fontSize: 16,
      padding: "10px 10px 10px 20px !important",
      transition: theme.transitions.create(["border-color", "box-shadow"]),
      background: `${theme.palette.primary.light}!important`,

      [`&:focus`]: {
        borderRadius: 100,
        boxShadow: "0 0 0 0.2rem rgba(0,123,255,.25)",
      },
    },
  })
)(InputBase)

export default BootstrapInput
