import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"

import ICO_ArrowUp from "assets/icons/arrow-up.png"
import ICO_ArrowDown from "assets/icons/arrow-down.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  dropdown: {
    "width": "40px",
    "height": "40px",
    "borderRadius": "100%",
    "background": "linear-gradient(180deg, #73D6F1 0%, #5F72FF 100%)",
    "cursor": "pointer",
    "color": palette.common.white,
    "flexDirection": "row",
    "justifyContent": "center",
    "alignItems": "center",
    "display": "flex",
    "padding": "20px",

    "& img": {
      width: "10px",

      [breakpoints.down("sm")]: {
        width: "10px",
      },
    },
  },
}))

export interface SwapButtonProps {
  style?: object
  onButtonClick: any
}

const SwapButton: React.FC<SwapButtonProps> = ({
  style = {},
  onButtonClick,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  return (
    <Box className={cx(classes.dropdown)} onClick={onButtonClick} style={style}>
      <img src={ICO_ArrowUp} alt={"<"} />
      <Box mx="1px"></Box>
      <img src={ICO_ArrowDown} alt={">"} />
    </Box>
  )
}

export default SwapButton
