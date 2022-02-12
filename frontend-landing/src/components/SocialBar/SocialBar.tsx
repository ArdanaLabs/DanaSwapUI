import React from "react"
import cx from "classnames"
import { Box, useMediaQuery, Link } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"

import { socials } from "data"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    width: "280px",

    [`& > .link:hover path`]: {
      fill: palette.secondary.main,
      transition: "all .2s",
    },

    [`& > .link svg`]: {
      width: "22px",
    },

    [breakpoints.down("xs")]: {
      width: "100%",
    },
  },
}))

const SocialBar: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      {socials.map((social, index) => (
        <Link
          className="link"
          href={social.url}
          key={index}
          rel="noopener noreferrer"
          target="_blank"
        >
          <social.image />
        </Link>
      ))}
    </Box>
  )
}

export default SocialBar
