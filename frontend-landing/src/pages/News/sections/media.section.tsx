import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import { useIsDarkMode } from "state/user/hooks"
import { MediaNewsBox } from "components"
import { NewsOnMediaList } from "data"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    paddingTop: "50px",
    paddingBottom: "50px",
  },
  title: {
    marginRight: "20px",
    [`& span`]: {
      color: palette.secondary.main,
    },
  },
}))

const MediaSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={classes.root}>
      <Container>
        <Grid container>
          <Grid item xs={12}>
            <Box display={"flex"} alignItems={"center"}>
              <Typography component="h3" variant="h3" className={classes.title}>
                <small>
                  Ardana on <span>Media</span>
                </small>
              </Typography>
            </Box>
          </Grid>
        </Grid>
        <Grid container spacing={3}>
          {NewsOnMediaList.slice(0, 6).map((news, i) => (
            <Grid item xs={12} sm={6} md={4} key={news.title + i}>
              <MediaNewsBox {...news} />
            </Grid>
          ))}
        </Grid>
      </Container>
    </Box>
  )
}

export default MediaSection
