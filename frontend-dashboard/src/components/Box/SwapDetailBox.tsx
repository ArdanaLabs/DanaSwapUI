import React from "react"
import { Box, useMediaQuery, Typography } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useUserTheme } from "state/user/hooks"
import * as Theme from "Data/User/Theme"

import CircleInfoCyanIcon from "assets/imgs/circle-info-cyan.png"
import CircleInfoDarkBlueIcon from "assets/imgs/circle-info-darkblue.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: palette.type === "light" ? "transparent" : "#0A104599",
    borderRadius: "5px",
    padding: 15,
    boxShadow:
      palette.type === "light"
        ? "2px 2px 10px rgba(0, 0, 0, 0.1)"
        : "0px 4px 4px rgba(0, 0, 0, 0.25)",
  },
  text: {
    color: palette.primary.main,
    lineHeight: "100%",
    fontWeight: 400,
  },
}))

interface Props {
  fromTokenDenom: string
  toTokenDenom: string
  fee: number
  slip: number
  tradeRoute: string
}

const SwapDetailBox: React.FC<Props> = ({
  fromTokenDenom,
  toTokenDenom,
  fee,
  slip,
  tradeRoute,
}) => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })
  const isDarkTheme: boolean = Theme.Eq.equals(userTheme, Theme.Theme.Dark)

  const renderRow = (text: string, content: string | number) => (
    <Box display="flex" justifyContent="space-between" alignItems="center">
      <Typography variant="h4" component="span" className={classes.text}>
        {text}
      </Typography>
      <Box display="flex" alignItems={"center"}>
        <Typography variant="h4" component="span" className={classes.text}>
          {content}
        </Typography>
        <Box ml={1}>
          <img
            src={isDarkTheme ? CircleInfoCyanIcon : CircleInfoDarkBlueIcon}
            alt="more info"
          />
        </Box>
      </Box>
    </Box>
  )

  return (
    <Box className={classes.root}>
      {renderRow(
        `Rate ${fromTokenDenom}/${toTokenDenom} <=> (including fees):`,
        fee
      )}
      {renderRow(`Slip:`, `${slip * 100}%`)}
      {renderRow(`Trade routed through:`, tradeRoute)}
    </Box>
  )
}

export default SwapDetailBox
