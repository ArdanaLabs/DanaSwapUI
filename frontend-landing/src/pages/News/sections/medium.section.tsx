import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Typography,
  Link,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import { useIsDarkMode } from "state/user/hooks"
import { NewsBox } from "components"
import { NewsOnMediumList } from "data"

import { ReactComponent as MediumIcon } from "assets/icons/medium.svg"

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
  badge: {
    textAlign: "center",
    cursor: "pointer",
    display: "inline-flex",
    alignItems: "center",
    borderRadius: "50px",
    background: "linear-gradient(89.62deg, #72D2F2 0.3%, #6077FF 99.64%)",
    textTransform: "uppercase",
    padding: "15px 30px",
  },
}))

const MediumSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const renderBadge = () => (
    <Link
      href="https://medium.com/ardana-hub"
      target="href"
      underline="none"
      className={classes.badge}
    >
      <Typography component="div" variant="button">
        Follow Ardana Hub On Medium
      </Typography>
      <Box ml={"10px"} lineHeight={"0"}>
        <MediumIcon />
      </Box>
    </Link>
  )

  return (
    <Box className={classes.root}>
      <Container>
        <Grid container>
          <Grid item xs={12}>
            <Box display={"flex"} alignItems={"center"}>
              <Typography component="h3" variant="h3" className={classes.title}>
                <small>
                  Ardana on <span>Medium</span>
                </small>
              </Typography>
              {!mobile && renderBadge()}
            </Box>
          </Grid>
        </Grid>
        <Grid container spacing={3}>
          {NewsOnMediumList.slice(0, !mobile ? 6 : 3).map((news) => (
            <Grid item xs={12} sm={6} md={4} key={news.title}>
              <NewsBox {...news} />
            </Grid>
          ))}
        </Grid>
        <Box display="flex" justifyContent="center">
          {mobile && renderBadge()}
        </Box>
      </Container>
    </Box>
  )
}

export default MediumSection
