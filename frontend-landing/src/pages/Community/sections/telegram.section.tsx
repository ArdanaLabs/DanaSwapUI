import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import { TelegramChannelBox } from "components"
import { useIsDarkMode } from "state/user/hooks"
import { TelegramChannelList } from "data"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    paddingTop: "150px",
    paddingBottom: "150px",
  },
  title: {
    [`& > span`]: {
      color: palette.secondary.main,
    },
  },
}))

const TelegramSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={classes.root}>
      <Container>
        <Grid container justifyContent="center">
          <Grid item xs={12} md={6}>
            <Box textAlign={"center"}>
              <Typography component="h3" variant="h3" className={classes.title}>
                <span>Telegram</span> Channels
              </Typography>
              <Box mb="20px" />
              <Typography component="h4" variant="h4">
                We have proper Telegram channels catering to different
                languages. Please follow through and find the proper channel for
                you.
              </Typography>
            </Box>
          </Grid>
        </Grid>
        <Box mb="50px" />
        <Grid container spacing={3}>
          {TelegramChannelList.map((channel) => (
            <Grid item xs={12} md={3} key={channel.country}>
              <TelegramChannelBox {...channel} />
            </Grid>
          ))}
        </Grid>
      </Container>
    </Box>
  )
}

export default TelegramSection
