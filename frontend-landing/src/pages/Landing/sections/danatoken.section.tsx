import React from "react"
import { Box, useMediaQuery, Container, Typography } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"

import BG_LEFT from "assets/backgrounds/danatoken-left-bg.png"
import BG_RIGHT from "assets/backgrounds/danatoken-right-bg.png"
import BG_CYAN_GRADIENT from "assets/backgrounds/cyan-gradient.png"

import BG_LEFT_MOBILE from "assets/backgrounds/danatoken-left-mobile-bg.png"
import BG_RIGHT_MOBILE from "assets/backgrounds/danatoken-right-mobile-bg.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    textAlign: "center",
    background: ` url(${BG_LEFT}) top left no-repeat,
                  url(${BG_RIGHT}) top 50% right no-repeat,
                  url(${BG_CYAN_GRADIENT}) top 50% right -200px no-repeat`,
    backgroundSize: "auto 700px, auto 50vh, 700px",
    height: "700px",
    display: "flex",
    alignItems: "center",
    justifyContent: "center",

    [breakpoints.down("xs")]: {
      background: ` url(${BG_LEFT_MOBILE}) top left no-repeat,
                    url(${BG_RIGHT_MOBILE}) bottom right no-repeat,
                    url(${BG_CYAN_GRADIENT}) bottom -300px right -300px no-repeat`,
      backgroundSize: "50vw, 30vw, cover",
      padding: "0px 50px",
    },
  },

  title: {
    lineHeight: "100%",

    [breakpoints.down("xs")]: {
      paddingTop: "200px",
    },

    [`& span`]: {
      color: palette.secondary.main,
    },
  },

  content: {
    lineHeight: "26px",

    [breakpoints.down("xs")]: {
      lineHeight: "18.4px",
    },
  },
}))

declare global {
  namespace JSX {
    interface IntrinsicElements {
      // "coingecko-coin-ticker-widget": React.DetailedHTMLProps<React.HTMLAttributes<HTMLElement>, HTMLElement>;
      "coingecko-coin-ticker-widget": {
        currency: string
        locale: string
        width: string
      }
    }
  }
}

const DanaswapSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Container maxWidth="md">
        <Typography variant="h3" component="h3" className={cx(classes.title)}>
          <span>DANA</span> Tokens
        </Typography>
        <Box mt="20px" />
        <Typography variant="h4" component="h4" className={cx(classes.content)}>
          The DANA token is the utility and governance token of the Ardana
          ecosystem which rewards holders with profits from Ardana and allows
          those who hold it to vote on changes to the project’s parameters.
        </Typography>
        <Box mt="20px" />
        <Box display="flex" justifyContent="center">
          {/* <coingecko-coin-ticker-widget
              coin-id="ardana"
              currency="usd"
              locale="en"
              background-color="#3d40eb"
              width="300"
            /> */}
        </Box>
      </Container>
    </Box>
  )
}

export default DanaswapSection
