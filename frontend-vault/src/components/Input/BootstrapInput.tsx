import { createStyles, InputBase, Theme, withStyles } from "@material-ui/core"

const BootstrapInput = withStyles((theme: Theme) =>
  createStyles({
    root: {
      "width": "100%",
      "& > svg": {
        display: "none",
      },
    },
    input: {
      "fontFamily": "Brandon Grotesque",
      "fontWeight": 900,
      "borderRadius": 100,
      "color": theme.palette.primary.main,
      "width": "100%",
      "position": "relative",
      "border": `1px solid ${theme.palette.primary.main}`,
      "fontSize": 16,
      "padding": "10px 10px 10px 20px",
      "transition": theme.transitions.create(["border-color", "box-shadow"]),
      "&:focus": {
        borderRadius: 100,
        boxShadow: "0 0 0 0.2rem rgba(0,123,255,.25)",
      },
    },
  })
)(InputBase)

export default BootstrapInput
