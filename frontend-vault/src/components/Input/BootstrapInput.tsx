import { createStyles, InputBase, Theme, withStyles } from "@material-ui/core"

const BootstrapInput = withStyles((theme: Theme) =>
  createStyles({
    root: {
      "width": "100%",
      "& > svg": {
        right: "10px",
        color: "white",
      },
    },
    input: {
      "fontFamily": "Brandon Grotesque",
      "fontWeight": 900,
      "borderRadius": 100,
      "color": theme.palette.common.white,
      "width": "100%",
      "position": "relative",
      "fontSize": 16,
      "padding": "10px 10px 10px 20px",
      "transition": theme.transitions.create(["border-color", "box-shadow"]),
      "background": theme.palette.primary.light,

      "&:focus": {
        borderRadius: 100,
        boxShadow: "0 0 0 0.2rem rgba(0,123,255,.25)",
      },
    },
  })
)(InputBase)

export default BootstrapInput
