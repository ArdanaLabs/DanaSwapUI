import { createStyles, Theme, WithStyles, withStyles } from "@material-ui/core/styles";
import MuiDialogTitle from '@material-ui/core/DialogTitle';
import { IconButton, Typography } from "@material-ui/core";
import CloseIcon from '@material-ui/icons/Close';

const styles = (theme: Theme) =>
  createStyles({
    root: {
      width: "450px",
      margin: 0,
      padding: "10px 0px",
      color: theme.palette.type === "light" ? theme.palette.primary.main : theme.palette.text.hint,

      "& h6" : {
        fontFamily: "Brandon Grotesque",
        fontStyle: "normal",
        fontWeight: 900,
        fontSize: "17px",
        lineHeight: "100%",

      }
    },
    closeButton: {
      color: theme.palette.secondary.main,
      position: "absolute",
      right: theme.spacing(1),
      top: theme.spacing(1),
    },
  });

export interface DialogTitleProps extends WithStyles<typeof styles> {
  id: string;
  children: React.ReactNode;
  onClose: () => void;
}

const DialogTitle = withStyles(styles)((props: DialogTitleProps) => {
  const { children, classes, onClose, ...other } = props;
  return (
    <MuiDialogTitle disableTypography className={classes.root} {...other}>
      <Typography variant="h6">{children}</Typography>
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
  );
});

export default DialogTitle;
