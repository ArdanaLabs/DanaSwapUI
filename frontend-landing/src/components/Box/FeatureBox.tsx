import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import ScrollAnimation from "react-animate-on-scroll";
import { useIsDarkMode } from "state/user/hooks";

export interface FeatureBoxProps {
  image?: any;
  title?: string;
  content?: string;
  custom_style?: object;
}

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    position: "relative",
    borderRadius: "10px",
    flex: 2,
    marginBottom: "30px",
    cursor: "pointer",
    transition: "transform .3s",
    textAlign: "center",

    "&:hover": {
      transform: "scale(1.05)",
    },
  },

  image: {
    position: "absolute",
    transform: "translate(-50%, -50%)",
    left: "50%",
    top: "-10px",
    "& > img": {
      width: "175px",
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
      fontSize: "24px",
    },
  },

  content: {
    color: "#F5FCFE",
    fontSize: "20px",
    lineHeight: "30px",
    fontWeight: 300,
    fontFamily: "Museo Sans",
    fontStyle: "normal",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
    },
  },
}));

const FeatureBox: React.FC<FeatureBoxProps> = ({
  image,
  title,
  content,
  custom_style,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <ScrollAnimation
      animateIn="fadeInUp"
      animateOnce={true}
      className={cx(classes.bg)}
      style={custom_style}
    >
      <Box className={cx(classes.image)}>
        <img src={image} alt="title" />
      </Box>

      <Box mt={!mobile ? "70px" : "50px"} />

      <Box className={cx(classes.title)}>{title}</Box>

      <Box mt="30px"></Box>

      <Box className={cx(classes.content)}>{content}</Box>

      <Box mt="30px"></Box>
    </ScrollAnimation>
  );
};

export default FeatureBox;
