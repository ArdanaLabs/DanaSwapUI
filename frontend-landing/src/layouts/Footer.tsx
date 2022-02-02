import React from "react"
import { Box, useMediaQuery, Link, Container } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import _ from "lodash"

import { useIsDarkMode } from "state/user/hooks"

import { externals } from "data"
import BG_WAVE from "assets/backgrounds/wave-gradient.png"
import BG_WAVE_MOBILE from "assets/backgrounds/wave-mobile180-bg.png"
import LOGO_BLUE from "assets/logo_blue.png"
import { GradientButton, SocialBar } from "components"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    "background": `url(${BG_WAVE}) top left no-repeat`,
    "backgroundSize": "100%",
    "padding": "150px 0px 60px",
    "display": "flex",
    "justifyContent": "space-between",
    "alignItems": "flex-start",

    "& span": {
      "&:first-child": {
        fontFamily: "Brandon Grotesque",
        fontSize: "20px",
        lineHeight: "36px",
        color: palette.secondary.main,
        marginBottom: "12px",
        fontWeight: 900,

        [breakpoints.down("xs")]: {
          fontSize: "20px",
          marginBottom: "10px",
        },
      },
      "& > a": {
        color: palette.common.white,
      },
      "fontFamily": "Museo Sans",
      "fontSize": "16px",
      "lineHeight": "24px",
      "cursor": "pointer",

      [breakpoints.down("xs")]: {
        fontSize: "16px",
        lineHeight: "20px",
      },
    },

    [breakpoints.down("xs")]: {
      flexDirection: "column",
      paddingLeft: "20px",
      paddingRight: "20px",
      textAlign: "center",
      alignItems: "center",
      background: `url(${BG_WAVE_MOBILE}) top -30px left no-repeat`,
      backgroundSize: "100%",
    },
  },

  container: {
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
    flexDirection: "column",
    textAlign: "center",
  },

  title: {
    color: palette.secondary.main,
    fontFamily: "Brandon Grotesque",
    fontWeight: 900,
    fontSize: "32px",
    lineHeight: "100%",
    [breakpoints.down("xs")]: {
      fontSize: "25px",
      lineHeight: "27.5px",
    },
  },

  logo: {
    "display": "inline-flex",
    "justifyContent": "center",
    "alignItems": "center",
    "width": "250px",
    "height": "250px",
    "padding": "40px",
    "position": "relative",
    "lineHeight": 0,

    "& > img": {
      borderRadius: "50%",
      [breakpoints.down("xs")]: {
        width: "60px",
      },
    },

    [breakpoints.down("xs")]: {
      width: "150px",
      height: "150px",
    },
  },

  photo: {
    position: "absolute",
    top: "50%",
    left: "50%",
    transform: "translate(-50%, -50%)",
    display: "inline-flex",
    justifyContent: "center",
    alignItems: "center",
    borderRadius: "50%",
    [breakpoints.down("xs")]: {
      width: "60px",
    },
  },

  guide: {
    "width": "100%",
    "display": "flex",
    "justifyContent": "space-between",
    "textAlign": "left",

    "& > .section": {
      display: "flex",
      flexDirection: "column",
      marginBottom: "20px",

      [breakpoints.down("xs")]: {
        width: "33%",
      },
    },

    [breakpoints.down("xs")]: {
      flexFlow: "wrap",
      textAlign: "center",
    },
  },

  socials: {
    display: "flex",
    flexDirection: "column",

    [breakpoints.down("xs")]: {
      width: "100%",
    },
  },
}))

const Footer: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Container maxWidth="md">
        <Box className={cx(classes.container)}>
          <Box className={cx(classes.title)}>
            Ardana is the leading stablecoin and stableswap DEX on Cardano
          </Box>
          <Box my={"30px"} className={cx(classes.logo)}>
            <GradientButton
              width={!mobile ? 175 : 100}
              height={!mobile ? 175 : 100}
              clickable={false}
            />
            <img className={cx(classes.photo)} src={LOGO_BLUE} alt="logo" />
          </Box>
          <Box className={cx(classes.guide)}>
            {_.keys(externals).map((group) => (
              <Box className="section" key={group}>
                <span>{group}</span>
                {_.keys(externals[group]).map((external) => (
                  <span key={external}>
                    <Link
                      rel="noopener noreferrer"
                      target={
                        externals[group][external].charAt(0) === "/"
                          ? "_self"
                          : "_blank"
                      }
                      href={externals[group][external]}
                      underline={
                        externals[group][external] === "#" ? "none" : "hover"
                      }
                      style={
                        externals[group][external] === "#"
                          ? { pointerEvents: "none" }
                          : { pointerEvents: "initial" }
                      }
                    >
                      {external}
                    </Link>
                  </span>
                ))}
              </Box>
            ))}
            <Box className={cx(classes.socials)}>
              <span>Our Socials</span>
              <span>
                Follow us to hear about Ardana
                <br />
                news and events.
              </span>

              <Box mt={"25px"} />
              <SocialBar />
            </Box>
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default Footer
