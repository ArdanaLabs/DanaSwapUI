import React, { useEffect } from "react"
import Carousel from "react-elastic-carousel"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Typography,
  Link,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { TwitterClient } from "twitter-api-client"

import { useIsDarkMode } from "state/user/hooks"
import { TwitterNewsBox } from "components"
import { NewsOnTwitterList } from "data"

import { ReactComponent as TwitterIcon } from "assets/icons/twitter.svg"

const twitterClient = new TwitterClient({
  apiKey: "jnTPb81hoJWcI0ipkj5aJZTGC",
  apiSecret: "NobWgqETxHE7RsxJwvH3kQOuDXB4MYn1cr65ikyxlkkWPkFt6Y",
  accessToken: "858267119229689857-g64vncq9OBDcy1U3vwwteCAUmoo6hw9",
  accessTokenSecret: "fv0RMswaVrfKq1vkuNyBJK9kH35nydoTyr8ZjoXqrpHpf",
})

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
  panel: {
    background: palette.background.paper,
    borderRadius: "10px",
    paddingLeft: "30px",
    paddingRight: "30px",
    marginTop: "30px",
    marginBottom: "30px",

    [breakpoints.down("xs")]: {
      paddingLeft: "10px",
      paddingRight: "10px",
    },
  },
}))

const TwitterSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  useEffect(() => {
    ;(async () => {
      // const response = await fetch(url, {
      //   method: 'POST', // *GET, POST, PUT, DELETE, etc.
      //   mode: 'cors', // no-cors, *cors, same-origin
      //   cache: 'no-cache', // *default, no-cache, reload, force-cache, only-if-cached
      //   credentials: 'same-origin', // include, *same-origin, omit
      //   headers: {
      //     'Content-Type': 'application/json'
      //     // 'Content-Type': 'application/x-www-form-urlencoded',
      //   },
      //   redirect: 'follow', // manual, *follow, error
      //   referrerPolicy: 'no-referrer', // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
      //   body: JSON.stringify(data) // body data type must match "Content-Type" header
      // });

      const data = await twitterClient.accountsAndUsers.usersSearch({
        q: "twitterDev",
      })

      console.log(111111, data)
    })()
  }, [])

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
        {/* <Box className={classes.panel} mt="30px">
          <Grid container spacing={3}>
            {NewsOnTwitterList.slice(0, 3).map((news) => (
              <Grid item xs={12} sm={6} md={4} key={news.title}>
                <TwitterNewsBox {...news} />
              </Grid>
            ))}
          </Grid>
        </Box> */}
        <Carousel
          className={classes.panel}
          itemsToShow={!mobile ? 3 : 1}
          isRTL={false}
          itemPadding={[!mobile ? 10 : 5]}
          renderArrow={() => <></>}
          renderPagination={() => <></>}
        >
          {NewsOnTwitterList.slice(0, 8).map((news, i: number) => (
            <TwitterNewsBox key={news.title + i} {...news} />
          ))}
        </Carousel>
        <Box display="flex" justifyContent="center">
          {mobile && renderBadge()}
        </Box>
      </Container>
    </Box>
  )
}

export default TwitterSection
