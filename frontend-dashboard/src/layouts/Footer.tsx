import React from "react"
import { Box, useMediaQuery, Container, Link } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import { useIsDarkMode } from "state/user/hooks"
import LogoLight from "assets/logo-light.png"
import { ReactComponent as TwitterIcon } from "assets/icons/twitter.svg"
import { ReactComponent as DiscordIcon } from "assets/icons/discord.svg"
import { ReactComponent as LinkedinIcon } from "assets/icons/linkedin.svg"
import { ReactComponent as MediumIcon } from "assets/icons/medium.svg"
import { ReactComponent as RedisIcon } from "assets/icons/redis.svg"
import { ReactComponent as TelegramIcon } from "assets/icons/telegram.svg"

const Socials = [
  {
    name: "Twitter",
    icon: TwitterIcon,
    link: "https://google.com",
  },
  {
    name: "Discord",
    icon: DiscordIcon,
    link: "https://google.com",
  },
  {
    name: "Telegram",
    icon: TelegramIcon,
    link: "https://google.com",
  },
  {
    name: "Medium",
    icon: MediumIcon,
    link: "https://google.com",
  },
  {
    name: "Redis",
    icon: RedisIcon,
    link: "https://google.com",
  },
  {
    name: "Linkedin",
    icon: LinkedinIcon,
    link: "https://google.com",
  },
]

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: "#1F297B",
    marginTop: "50px",
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
    },

    [`& svg:hover path`]: {
      fill: palette.secondary.main,
    },
  },
}))

const Footer: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={classes.root}>
      <Container>
        <Box className={classes.container}>
          <Link href="/" className={classes.logo}>
            <img src={LogoLight} alt="logo" height={"50px"} />
          </Link>
          <Box className={classes.socialLinks}>
            {Socials.map((social) => (
              <Link key={social.name} href={social.link} target="_blank">
                <social.icon />
              </Link>
            ))}
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default Footer
