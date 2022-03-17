import React from "react"
import cx from "classnames"
import { Box, useMediaQuery, Typography } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"

import CircleInfoCyanIcon from "assets/imgs/circle-info-cyan.svg"
import CircleInfoDarkBlueIcon from "assets/imgs/circle-info-darkblue.svg"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    width: "100%",
    position: "relative",
    background: palette.background.paper,
    borderRadius: "10px",
    height: 100,
  },
  leftBorder: {
    position: "absolute",
    left: 0,
    top: 0,
    bottom: 0,
    height: "100%",
    background: `linear-gradient(180deg, ${palette.secondary.main} 0%, ${palette.secondary.dark} 100%)`,
    borderRadius: "10px",
  },
  info: {
    position: "absolute",
    top: 3,
    right: 10,
  },
  display: {
    padding: "25px 0px 10px 20px",

    [`& p:first-child`]: {
      color: palette.secondary.main,
      whiteSpace: "pre-line",
      textTransform: "uppercase",
      lineHeight: "100%",
      marginBottom: "10px",
    },
    [`& p:last-child`]: {
      color: palette.primary.main,
    },
  },
}))

export interface OverViewBoxProps {
  label: string
  content: string
}

const OverViewBox: React.FC<OverViewBoxProps> = ({ label, content }) => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const isDarkTheme: boolean = Theme.Eq.equals(userTheme, Theme.Theme.Dark)
  const classes = useStyles({ dark: isDarkTheme, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Box className={cx(classes.leftBorder)}>&nbsp;&nbsp;</Box>
      <Box className={cx(classes.info)}>
        <img
          src={isDarkTheme ? CircleInfoCyanIcon : CircleInfoDarkBlueIcon}
          alt="info"
        />
      </Box>

      <Box className={cx(classes.display)}>
        <Typography variant="body2" component="p">
          {label}
        </Typography>
        <Typography variant="subtitle2" component="p">
          {content}
        </Typography>
      </Box>
    </Box>
  )
}

export default OverViewBox
