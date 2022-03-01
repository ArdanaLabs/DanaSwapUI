import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"

import ArrowLeftRightIcon from "assets/icons/arrow-leftright.svg"

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
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={classes.root} onClick={handleClick}>
      <img src={ArrowLeftRightIcon} alt="swp" />
    </Box>
  )
}

export default SwapButton
