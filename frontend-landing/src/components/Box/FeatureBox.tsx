import React, { useState } from "react"
import cx from "classnames"
import ReactPlayer from "react-player"
import { Box, Typography, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"
import { GradientButton } from "components/Button"

const heroVideo = "https://youtu.be/9lMoZ0APtTQ"

export interface FeatureBoxProps {
  image?: any
  title?: string
  content?: string
}

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
    borderRadius: "10px",
    cursor: "pointer",
    height: "310px",
    marginTop: "65px",

    [breakpoints.down("xs")]: {
      height: "160px",
      marginLeft: "10px",
      marginRight: "10px",
    },
  },
  video: {
    "position": "absolute",
    "top": 0,
    "left": 0,
    "height": "100%",
    "width": "100%",
    "& > div": {
      "& video": {
        objectFit: "cover",
        borderRadius: "10px",
        transform: "rotateY(180deg)",
      },
    },
  },
  bg: {
    flex: 2,
    textAlign: "center",
    background: "rgba(24, 34, 113, 0.6)",
    borderRadius: "10px",
    padding: "20px",
    opacity: 0.99,
    height: "100%",
    [breakpoints.down("xs")]: {
      padding: "30px",
    },
  },

  image: {
    position: "absolute",
    transform: "translate(-50%, -50%)",
    left: "50%",
    top: "-10px",
    lineHeight: 0,
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

  title: {
    lineHeight: "110%",
  },

  content: {
    lineHeight: "24px",

    [breakpoints.down("sm")]: {
      lineHeight: "19.2px",
    },
  },
}))

const FeatureBox: React.FC<FeatureBoxProps> = ({ image, title, content }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const [hover, setHover] = useState<boolean>(false)

  return (
    <Box className={cx(classes.root)}>
      {!mobile && (
        <Box
          className={cx(classes.video)}
          visibility={hover ? "visible" : "hidden"}
          style={hover ? { opacity: 1 } : { opacity: 0 }}
        >
          <ReactPlayer
            url={heroVideo}
            playing={false}
            loop={true}
            muted
            width="100%"
            height="100%"
            playbackRate={0.5}
            style={hover ? { visibility: "visible" } : { visibility: "hidden" }}
          />
        </Box>
      )}

      <Box
        className={cx(classes.bg)}
        onMouseEnter={() => {
          setHover(true)
        }}
        onMouseLeave={() => {
          setHover(false)
        }}
      >
        <Box className={cx(classes.image)}>
          <GradientButton
            width={!mobile ? 145 : 81}
            height={!mobile ? 145 : 81}
            clickable={false}
          />
          <img className={cx(classes.photo)} src={image} alt="title" />
        </Box>

        <Box mt={!mobile ? "80px" : "25px"} />

        <Typography variant="h5" component="h5" className={cx(classes.title)}>
          {title}
        </Typography>

        <Box mt={!mobile ? "30px" : "10px"} />

        <Typography variant="h4" component="h4" className={cx(classes.content)}>
          {content}
        </Typography>

        <Box mt="30px"></Box>
      </Box>
    </Box>
  )
}

export default FeatureBox
