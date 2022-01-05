import React from "react"
import cx from "classnames"
import { Box, Typography, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"

import ArdanaLogo from "assets/logo_blue.png"
import TwitterLogo from "assets/icons/twitter.svg"

interface TwitterNewsBoxProps {
  title: string
  type: string
  content: string
  image?: string
  datetime: string
}

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
    marginTop: "30px",
    marginBottom: "30px",
    padding: "20px",
    borderRadius: "10px",
    background:
      "linear-gradient(180.2deg, rgba(98, 126, 255, 0.2) 0.17%, rgba(77, 97, 210, 0) 116.51%)",
  },
  logo: {
    background: palette.common.white,
    borderRadius: "100%",
    padding: "10px",
    width: "50px",
    height: "50px",

    marginRight: "20px",
  },

  content: {
    whiteSpace: "pre-line",

    [`& span`]: {
      color: palette.secondary.main,
    },
  },

  twitterLogo: {
    position: "absolute",
    top: "25px",
    right: "20px",
  },
}))

const TwitterNewsBox: React.FC<TwitterNewsBoxProps> = ({
  title,
  type,
  content,
  image = undefined,
  datetime,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <img src={TwitterLogo} alt="twitter" className={classes.twitterLogo} />
      <Box display={"flex"}>
        <img src={ArdanaLogo} alt="ardana" className={classes.logo} />
        <Box>
          <Typography component="h5" variant="h5">
            <small>{title}</small>
          </Typography>
          <Typography component="h4" variant="h4">
            <small>{type}</small>
          </Typography>
        </Box>
      </Box>

      <Box mt={"20px"} />

      <Typography component="h4" variant="h4" className={classes.content}>
        <small
          dangerouslySetInnerHTML={{
            __html: content,
          }}
        />
      </Typography>

      {image && (
        <Box mt={"20px"}>
          <img src={image} alt="news" width="100%" />
        </Box>
      )}

      <Box mt="10px" />

      <Typography component="h4" variant="h4">
        <small>{datetime}</small>
      </Typography>
    </Box>
  )
}

export default TwitterNewsBox
