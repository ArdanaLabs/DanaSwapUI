import React from "react"
import { Box, useMediaQuery, Container, Link } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"

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

  // TODO: change container to use `gap` instead of this `margin-right` hack
  socialIconLink: {
    "display": "inline-flex",
    "height": "2.25em",
    "width": "2.25em",
    "alignItems": "center",
    "justifyContent": "center",
    "borderRadius": "50%",
    "backgroundColor": "white",
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
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <Box className={cx(classes.container)}>
          <Box className={cx(classes.logo)}>
            {/* TODO: make this one image so the titles make sense; link to the homepage */}
            <img src={LOGO_White} alt="Ardana" />
            <img src={LOGO_Text} alt="" />
          </Box>
          {/* TODO: style this as flex or grid; aria labels; titles */}
          <Box>
            <Link
              className={cx(classes.socialIconLink)}
              href="#"
              rel="noopener"
            >
              <i className="fab fa-twitter"></i>
            </Link>
            <Link
              className={cx(classes.socialIconLink)}
              href="#"
              rel="noopener"
            >
              <i className="fab fa-instagram"></i>
            </Link>
            <Link
              className={cx(classes.socialIconLink)}
              href="#"
              rel="noopener"
            >
              <i className="fab fa-medium"></i>
            </Link>
            <Link
              className={cx(classes.socialIconLink)}
              href="#"
              rel="noopener"
            >
              <i className="fab fa-youtube"></i>
            </Link>
            <Link
              className={cx(classes.socialIconLink)}
              href="#"
              rel="noopener"
            >
              <i className="fab fa-linkedin"></i>
            </Link>
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default Footer
