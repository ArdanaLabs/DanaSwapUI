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
import TwitterImage from "assets/backgrounds/twitter.png"
import { ReactComponent as TwitterIcon } from "assets/icons/twitter.svg"
import { GradientBox } from "components"

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
  h3: {
    [`& > span`]: {
      color: palette.secondary.main,
    },
  },
}))

const TwitterSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const renderBadge = () => (
    <Link
      href="https://twitter.com/ardanaproject"
      target="_blank"
      underline="none"
      className={classes.badge}
    >
      <Typography component="div" variant="button">
        Follow Ardana on Twitter
      </Typography>
      <Box ml={"10px"} lineHeight={"0"}>
        <TwitterIcon />
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
                  Ardana on <span>Twitter</span>
                </small>
              </Typography>
              {!mobile && renderBadge()}
            </Box>
          </Grid>
        </Grid>

        <Box my={"100px"}>
          <Grid container justifyContent="center">
            <Grid item xs={12} md={9}>
              <Grid
                container
                spacing={3}
                direction={!mobile ? "row" : "column-reverse"}
              >
                <Grid item container xs={12} md={6}>
                  <Box
                    display="flex"
                    flexDirection="column"
                    justifyContent={"center"}
                    textAlign={!mobile ? "start" : "center"}
                  >
                    <Typography
                      component="h3"
                      variant="h3"
                      className={classes.h3}
                    >
                      Stay <span>updated.</span>
                    </Typography>
                    <Box mb="10px" />
                    <Typography component="h4" variant="h4">
                      Never miss a chance to connect! For the live news and
                      updates, follow Ardana on twitter.
                    </Typography>
                    <Box mb="30px" />
                    <Link
                      href="https://twitter.com/ardanaproject"
                      target="_blank"
                      underline="none"
                    >
                      <GradientBox width={245} height={40}>
                        <TwitterIcon width={"15px"} />
                        <Typography
                          component="div"
                          variant="button"
                          style={{ lineHeight: 0, marginLeft: "10px" }}
                        >
                          Follow us on Twitter
                        </Typography>
                      </GradientBox>
                    </Link>
                  </Box>
                </Grid>
                <Grid item xs={12} md={6}>
                  <Box textAlign="center">
                    <img src={TwitterImage} alt="twitter" width={"100%"} />
                  </Box>
                </Grid>
              </Grid>
            </Grid>
          </Grid>
        </Box>
        <Box display="flex" justifyContent="center">
          {mobile && renderBadge()}
        </Box>
      </Container>
    </Box>
  )
}

export default TwitterSection
