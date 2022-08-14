import React from "react"
import { useHistory } from "react-router-dom"

import DUSD_LOGO_BLUE from "assets/image/DUSD-LOGO-BLUE.png"
import DUSD_LOGO_WHITE from "assets/image/DUSD-LOGO-WHITE.png"
import BG_BLUE from "assets/image/backgrounds/BG-FOOTER-BLUE.png"
import BG_WHITE from "assets/image/backgrounds/BG-FOOTER-WHITE.png"
import { socials } from "data"
import { makeStyles } from "@mui/styles"
import { Box, Container, Link, Theme, useTheme } from "@mui/material"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    background: `url(${
      theme.palette.mode === "dark" ? BG_BLUE : BG_WHITE
    }) center center no-repeat`,
    backgroundSize: "cover",
    marginTop: "50px",
    filter: "drop-shadow(35px 0px 15px rgba(0, 0, 0, 0.12))",
  },

  logo: {
    paddingLeft: "10px",
    display: "flex",
    alignItems: "center",
    cursor: "pointer",
    [`& img`]: {
      width: "60px",

      [theme.breakpoints.down("sm")]: {
        marginBottom: "30px",
      },
    },
  },

  container: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    transition: "background .2s ease-in",
    padding: "20px 0px",

    [theme.breakpoints.down("sm")]: {
      flexDirection: "column",
    },
  },

  socials: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    width: "300px",

    [`& > .link`]: {
      lineHeight: 0,
    },

    [`& > .link svg`]: {
      width: "25px",
    },
    [`& > .link path`]: {
      fill: theme.palette.primary.main,
    },
  },
}))

const Footer: React.FC = () => {
  const theme = useTheme()
  const classes = useStyles(theme)
  const history = useHistory()

  return (
    <Box className={classes.root}>
      <Container>
        <Box className={classes.container}>
          <Box className={classes.logo} onClick={() => history.push("/")}>
            <img
              src={
                theme.palette.mode === "dark" ? DUSD_LOGO_WHITE : DUSD_LOGO_BLUE
              }
              alt=""
            />
          </Box>
          <Box className={classes.socials}>
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
        </Box>
      </Container>
    </Box>
  )
}

export default Footer
