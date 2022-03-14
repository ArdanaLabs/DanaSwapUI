import {
  Dialog,
  Box,
  useTheme,
  useMediaQuery,
  makeStyles,
  Button,
  styled,
  Typography,
} from "@material-ui/core"
import React from "react"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"

import CloseIcon from "assets/image/svgs/close.svg"
import { useHistory } from "react-router-dom"
import { ModalUIState } from "state/ui/reducer"

const StyledDialog = styled(Dialog)(({ theme }) => ({
  "& .MuiDialog-paper": {
    borderRadius: "20px",
  },
}))

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: palette.background.paper,
    padding: "50px 25px",
    maxWidth: "400px",
    position: "relative",
  },
  close: {
    position: "absolute",
    top: "20px",
    right: "20px",
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
    background: palette.info.light,
    borderRadius: "100%",
    boxShadow: "5px 5px 10px rgba(18, 36, 82, 0.25)",
    width: "30px",
    minWidth: "30px",
    height: "30px",
    cursor: "pointer",
  },

  body: {
    [`& .title, & .label, & .content`]: {
      color: palette.primary.main,
    },
    [`& .label`]: {
      textTransform: "uppercase",
    },
    [`& .action`]: {
      background: palette.info.light,
      borderRadius: "100px",
      width: "100%",
      padding: "10px",

      [`& h5`]: {
        color: palette.common.white,
        textTransform: "uppercase",
      },
    },

    [`& .divider`]: {
      border: `1px solid ${palette.info.dark}`,
      opacity: 0.6,
    },
  },
}))

interface Props {
  info: ModalUIState
  handleClose: () => void
}

const VaultModal: React.FC<Props> = ({ info, handleClose }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
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
      <Box className={cx(classes.root)}>
        <Button className={cx(classes.close)} onClick={handleClose}>
          <img src={CloseIcon} alt="close" />
        </Button>

        <Box className={cx(classes.body)} mt={"10px"}>
          <Typography variant="h3" component="h3" className="title">
            What do you want to do
            <br />
            with your ETH?
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
