import React from "react"
import { Box, useMediaQuery, Container, Link } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"
import LogoLight from "assets/logo-light.png"

import SocialMediasIcon from "assets/imgs/social-medias.svg"

const Socials = [
  {
    name: "twitter",
    link: "https://twitter.com/ardanaproject",
  },
  {
    name: "discord",
    link: "https://discord.gg/c9skrZvsqH",
  },
  {
    name: "telegram",
    link: "https://t.me/ardanaofficial",
  },
  {
    name: "medium",
    link: "https://medium.com/ardana-hub",
  },
  {
    name: "youtube",
    link: "https://www.youtube.com/channel/UCuVtpKzlmsD6s0ZiC0hakkA",
  },
  {
    name: "linkedin",
    link: "https://www.linkedin.com/company/ardanalabs/",
  },
]

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: "#1F297B",
    padding: 30,

    [breakpoints.down("xs")]: {
      padding: 50,
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

  logo: {
    [breakpoints.down("xs")]: {
      paddingBottom: 20,
    },
  },

  socialLinks: {
    width: "300px",
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",

    [`& a`]: {
      lineHeight: 0,

      [`& > .icon`]: {
        width: 26,
        height: 21,
        fill: palette.common.white,

        [`&:hover`]: {
          fill: palette.secondary.main,
        },
      },
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
    <Box className={classes.root}>
      <Container>
        <Box className={classes.container}>
          <Link href="/" className={classes.logo}>
            <img src={LogoLight} alt="" height={"50px"} />
          </Link>
          <Box className={classes.socialLinks}>
            {Socials.map((social) => (
              <Link key={social.name} href={social.link} target="_blank">
                {/* <social.icon /> */}
                <svg
                  xmlns="http://www.w3.org/2000/svg"
                  xmlnsXlink="http://www.w3.org/1999/xlink"
                  className="icon"
                >
                  <use xlinkHref={`${SocialMediasIcon}#${social.name}`} />
                </svg>
              </Link>
            ))}
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default Footer
