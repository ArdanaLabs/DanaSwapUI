import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { GradientBox, NewsBox } from "components"
import { NewsList } from "data"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    paddingTop: "50px",
    paddingBottom: "50px",
  },
  title: {
    [`& span`]: {
      color: palette.secondary.main,
    },
  },
}))

const CommunitySection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container spacing={3} justifyContent="center">
          <Grid item xs={12} md={5}>
            <Box
              display={"flex"}
              flexDirection={"column"}
              alignItems={"center"}
              textAlign="center"
            >
              <Typography
                component="h3"
                variant="h3"
                className={cx(classes.title)}
              >
                Ardana <span>News</span>
              </Typography>
              <Box mb="30px" />
              <Typography component="h4" variant="h4">
                For the latest news, make sure to follow Ardana’s social media
                accounts and keep checking back in to the Ardana platform.
              </Typography>
              <Box mb="30px" />
              <GradientBox width={145} height={40}>
                <Typography component="div" variant="button">
                  View More
                </Typography>
              </GradientBox>
            </Box>
          </Grid>

          <Grid item container xs={12} spacing={3}>
            {NewsList.map((news) => (
              <Grid item xs={12} md={4} key={news.title}>
                <NewsBox {...news} />
              </Grid>
            ))}
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default CommunitySection
