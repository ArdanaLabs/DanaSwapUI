import {
  createStyles,
  Theme,
  WithStyles,
  withStyles,
} from "@material-ui/core/styles"
import MuiDialogTitle from "@material-ui/core/DialogTitle"
import { IconButton } from "@material-ui/core"
import CloseIcon from "@material-ui/icons/Close"
import { FontFamilies } from "data"

const styles = (theme: Theme) =>
  createStyles({
    root: {
      width: "450px",
      textTransform: "uppercase",
      margin: 0,
      padding: "10px 0px",
      color: theme.palette.primary.main,

      [`& h6`]: {
        fontFamily: FontFamilies.Brandon,
        fontStyle: "normal",
        fontWeight: 900,
        fontSize: "15px",
        lineHeight: "100%",
      },
    },
    closeButton: {
      color: theme.palette.secondary.main,
      position: "absolute",
      right: theme.spacing(1),
      top: theme.spacing(1),
    },
  })

export interface DialogTitleProps extends WithStyles<typeof styles> {
  id: string
  children: React.ReactNode
  onClose: () => void
}

const DialogTitle = withStyles(styles)((props: DialogTitleProps) => {
  const { children, classes, onClose, ...other } = props
  return (
    <MuiDialogTitle className={classes.root} {...other}>
      {children}
      {onClose ? (
        <IconButton
          aria-label="close"
          className={classes.closeButton}
          onClick={onClose}
        >
          <CloseIcon />
        </IconButton>
      ) : null}
    </MuiDialogTitle>
  )
})

export default DialogTitle
