import React, { useState } from "react";
import cx from "classnames";
import ReactPlayer from "react-player";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

const heroVideo =
  "https://background.sfo3.digitaloceanspaces.com/stablecoin/output.m3u8";

export interface FeatureBoxProps {
  image?: any;
  title?: string;
  content?: string;
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
    },
  },
  video: {
    position: "absolute",
    top: 0,
    left: 0,
    height: "100%",
    width: "100%",
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
  },

  image: {
    position: "absolute",
    transform: "translate(-50%, -50%)",
    left: "50%",
    top: "-10px",
    "& > img": {
      width: "175px",

      [breakpoints.down("xs")]: {
        width: "100px",
      },
    },
  },

  title: {
    color: "#FFFFFF",
    // whiteSpace: "pre-line",
    fontSize: "30px",
    fontWeight: 900,
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    lineHeight: "110%",

    [breakpoints.down("sm")]: {
      whiteSpace: "unset",
      fontSize: "25px",
    },
  },

  content: {
    color: "#F5FCFE",
    fontSize: "20px",
    lineHeight: "26px",
    fontWeight: 300,
    fontFamily: "Museo Sans",
    fontStyle: "normal",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
      lineHeight: "19.2px",
    },
  },
}));

const FeatureBox: React.FC<FeatureBoxProps> = ({
  image,
  title,
  content,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  const [hover, setHover] = useState<boolean>(false);

  return (
    <Box className={cx(classes.root)}>
      <Box
        className={cx(classes.video)}
        visibility={hover ? "visible" : "hidden"}
      >
        <ReactPlayer
          url={heroVideo}
          playing
          loop={true}
          muted
          width="100%"
          height="100%"
          playbackRate={0.5}
        />
      </Box>

      <Box
        className={cx(classes.bg)}
        onMouseEnter={() => {
          setHover(true);
        }}
        onMouseLeave={() => {
          setHover(false);
        }}
      >
        <Box className={cx(classes.image)}>
          <img src={image} alt="title" />
        </Box>

        <Box mt={!mobile ? "70px" : "25px"} />

        <Box className={cx(classes.title)}>{title}</Box>

        <Box mt={!mobile ? "30px" : "10px"} />

        <Box className={cx(classes.content)}>{content}</Box>

        <Box mt="30px"></Box>
      </Box>
    </Box>
  );
};

export default FeatureBox;
