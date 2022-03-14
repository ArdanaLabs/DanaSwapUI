import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"

import ArrowLeftRightIcon from "assets/imgs/arrow-leftright.svg"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    width: "40px",
    height: "40px",
    borderRadius: "100%",
    background: "linear-gradient(180deg, #2E49C5 0%, #6480FF 106.6%)",
    cursor: "pointer",
    justifyContent: "center",
    alignItems: "center",
    display: "flex",
    padding: "20px",
  },
}))

export interface SwapButtonProps {
  handleClick: () => void
}

const SwapButton: React.FC<SwapButtonProps> = ({ handleClick }) => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))

  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

  return (
    <Box className={classes.root} onClick={handleClick}>
      <img src={ArrowLeftRightIcon} alt="swp" />
    </Box>
  )
}

export default SwapButton
