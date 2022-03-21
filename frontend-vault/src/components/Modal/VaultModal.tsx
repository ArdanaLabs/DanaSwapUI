import React from "react"
import {
  Box,
  Dialog,
  styled,
  Theme,
  Typography,
  useTheme,
  Button,
} from "@mui/material"
import { makeStyles } from "@mui/styles"

import CloseIcon from "assets/image/svgs/close.svg"
import { useHistory } from "react-router-dom"
import { ModalUIState } from "state/ui/reducer"

const StyledDialog = styled(Dialog)(() => ({
  [`& .MuiDialog-paper`]: {
    borderRadius: "20px",
  },
}))

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    background: theme.palette.background.paper,
    padding: "50px 25px",
    maxWidth: "400px",
    position: "relative",
  },
  close: {
    position: "absolute!important" as "absolute",
    top: "20px",
    right: "20px",
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
    background: theme.palette.info.light,
    borderRadius: "100%!important" as "100%",
    boxShadow: "5px 5px 10px rgba(18, 36, 82, 0.25)",
    width: "30px",
    minWidth: "30px!important" as "30px",
    height: "30px",
    cursor: "pointer",
  },

  body: {
    [`& .title, & .label, & .content`]: {
      color: theme.palette.primary.main,
    },
    [`& .title`]: {
      fontSize: 30,
      [theme.breakpoints.down("sm")]: {
        fontSize: 25,
      },
    },
    [`& .label`]: {
      textTransform: "uppercase",
    },
    [`& .action`]: {
      background: theme.palette.info.light,
      borderRadius: "100px",
      width: "100%",
      padding: "10px",

      [`& h5`]: {
        color: theme.palette.common.white,
        textTransform: "uppercase",
      },
    },

    [`& .divider`]: {
      borderStyle: "solid",
      borderWidth: 1,
      borderColor: theme.palette.info.dark,
      opacity: 0.6,
    },
  },
}))

interface Props {
  info: ModalUIState
  handleClose: () => void
}

const VaultModal: React.FC<Props> = ({ info, handleClose }) => {
  const theme = useTheme()
  const classes = useStyles(theme)
  const history = useHistory()

  const handleMultiply = () => {
    history.push(`/vaults/open-multiply/${info.type}`)
    handleClose()
  }

  const handleBorrow = () => {
    history.push(`/vaults/open-borrow/${info.type}`)
    handleClose()
  }

  return (
    <StyledDialog
      open={info.open}
      onClose={handleClose}
      aria-labelledby="alert-dialog-title"
      aria-describedby="alert-dialog-description"
    >
      <Box className={classes.root}>
        <Button className={classes.close} onClick={handleClose}>
          <img src={CloseIcon} alt="close" />
        </Button>

        <Box className={classes.body} mt={"10px"}>
          <Typography variant="h3" component="h3" className="title">
            {`What do you want to do\nwith your ETH?`}
          </Typography>

          <Box mt="15px" />

          <>
            <Typography variant="h5" component="h5" className="label">
              Multiply my ETH
            </Typography>
            <Box mt="10px" />
            <Typography variant="h6" component="h6" className="content">
              Create a Multiply position and get up to 3.22x exposure on your
              ETH.
            </Typography>
            <Box mt="10px" />
            <Button className="action" onClick={handleMultiply}>
              <Typography variant="h5" component="h5">
                Myltiply ETH
              </Typography>
            </Button>
          </>

          <Box className="divider" my={"20px"} />

          <>
            <Typography variant="h5" component="h5" className="label">
              Borrow against my ETH
            </Typography>
            <Box mt="10px" />
            <Typography variant="h6" component="h6" className="content">
              Borrow up to 50000 DAI for every $100,000 worth of ETH.
            </Typography>
            <Box mt="10px" />
            <Button className="action" onClick={handleBorrow}>
              <Typography variant="h5" component="h5">
                Borrow against ETH
              </Typography>
            </Button>
          </>
        </Box>
      </Box>
    </StyledDialog>
  )
}

export default VaultModal
