import React from "react"
import { Box, useMediaQuery, Container, Link } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"

import LOGO_White from "assets/logo_white.png"
import LOGO_Text from "assets/logo_text.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background:
      palette.type === "light"
        ? "linear-gradient(90.19deg, #2F3DA0 27.19%, #73D6F1 99.87%)"
        : palette.background.default,
    marginTop: "50px",
    padding: 10,
  },

  logo: {
    "paddingLeft": "10px",
    "display": "flex",
    "alignItems": "center",
    "cursor": "pointer",
    "& img": {
      padding: "20px 10px",
    },
  },

  container: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",

    [breakpoints.down("xs")]: {
      flexDirection: "column",
    },
  },

  socialIconLink: {
    "borderRadius": "50%",
    "backgroundColor": "white",
    "padding": "10px",
    "marginRight": "20px",
    "cursor": "pointer",
    "color": "gray",
    "textAlign": "center",
    "transition": "background .2s",
    "fontFamily": "auto",

    "& i": {
      width: "16px",
      height: "16px",
    },

    "&:hover": {
      backgroundColor: "lightgray",
    },
  },
}))

const Footer: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <Box className={cx(classes.container)}>
          <Box className={cx(classes.logo)}>
            <img src={LOGO_White} alt="logo" />
            <img src={LOGO_Text} alt="logo" />
          </Box>
          <Box>
            <Link className={cx(classes.socialIconLink)} href="#">
              <i className="fab fa-twitter"></i>
            </Link>
            <Link className={cx(classes.socialIconLink)} href="#">
              <i className="fab fa-instagram"></i>
            </Link>
            <Link className={cx(classes.socialIconLink)} href="#">
              <i className="fab fa-medium"></i>
            </Link>
            <Link className={cx(classes.socialIconLink)} href="#">
              <i className="fab fa-youtube"></i>
            </Link>
            <Link className={cx(classes.socialIconLink)} href="#">
              <i className="fab fa-linkedin"></i>
            </Link>
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default Footer
