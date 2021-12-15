import { Box, Container, Typography, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import BACKGROUND_WAVE_BLUE_M from "assets/image/backgrounds/BG-BLUE-MOBILE.png"
import BACKGROUND_WAVE_BLUE from "assets/image/backgrounds/BG-BLUE.png"
import BACKGROUND_WAVE_WHITE_M from "assets/image/backgrounds/BG-WHITE-MOBILE.png"
import BACKGROUND_WAVE_WHITE from "assets/image/backgrounds/BG-WHITE.png"
import cx from "classnames"
import { VaultStatCard } from "components/Card"
import React from "react"
import { useIsDarkMode } from "state/user/hooks"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    height: "500px",
    paddingTop: "0px",
    paddingBottom: "50px",
    background: `url(${
      palette.type === "dark" ? BACKGROUND_WAVE_BLUE : BACKGROUND_WAVE_WHITE
    }) right bottom no-repeat`,
    backgroundSize: "cover",

    [breakpoints.down("xs")]: {
      height: "400px",
      textAlign: "center",
      paddingBottom: "30px",
      background: `url(${
        palette.type === "dark"
          ? BACKGROUND_WAVE_BLUE_M
          : BACKGROUND_WAVE_WHITE_M
      }) right bottom no-repeat`,
    },
  },

  title: {
    color: palette.primary.main,
    textAlign: "center",
  },
}))

const AdSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box
      className={cx(classes.root)}
      display={"flex"}
      justifyContent={"center"}
      alignItems={"center"}
    >
      <Container>
        <Typography variant="h1" component="h1" className={cx(classes.title)}>
          Your Vaults
        </Typography>

        <VaultStatCard />
      </Container>
    </Box>
  )
}

export default AdSection
