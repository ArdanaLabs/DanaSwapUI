import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"

import ArrowRightIcon from "assets/image/icons/arrow-right-circle.svg"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
    borderRadius: "30px",
    padding: "50px 40px",
    color: palette.common.white,
    display: "flex",
    boxShadow: "10px 10px 30px rgba(0, 0, 0, 0.05)",
    cursor: "pointer",
    marginBottom: "30px",

    [breakpoints.down("xs")]: {
      padding: "20px 50px 100px 50px",
      textAlign: "center",
    },
  },
  typographyPrimary: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
  },
  typographySecondary: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 100,
  },
  title: {
    fontSize: "45px",
    lineHeight: "110%",
    marginBottom: "20px",

    [breakpoints.down("sm")]: {
      fontSize: "35px",
    },
  },
  content: {
    fontSize: "16px",
    lineHeight: "115%",
    whiteSpace: "pre-line",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
      whiteSpace: "unset",
    },
  },

  link: {
    "position": "absolute",
    "top": "50%",
    "right": "0px",
    "transform": "translate(-50%, -50%)",

    "& > img": {
      width: "50px",

      [breakpoints.down("xs")]: {
        width: "40px",
        transform: "rotateZ(90deg)",
      },
    },

    [breakpoints.down("xs")]: {
      top: "unset",
      bottom: "0px",
      left: "50%",
    },
  },
}))

export interface HelpCardProps {
  title: string
  content: string
  background: string
}

const HelpCard: React.FC<HelpCardProps> = ({ title, content, background }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)} style={{ background: background }}>
      <Box display="flex" flexDirection="column">
        <Box className={cx(classes.typographyPrimary, classes.title)}>
          {title}
        </Box>
        <Box className={cx(classes.typographySecondary, classes.content)}>
          {content}
        </Box>
      </Box>
      <Box className={cx(classes.link)}>
        <img src={ArrowRightIcon} alt="right" />
      </Box>
    </Box>
  )
}

export default HelpCard
