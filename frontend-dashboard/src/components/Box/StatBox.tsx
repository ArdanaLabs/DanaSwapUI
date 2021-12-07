import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import ScrollAnimation from "react-animate-on-scroll";
import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    position: "relative",
    borderRadius: "10px",
    marginBottom: "30px",
    cursor: "pointer",
    background: "linear-gradient(0deg, rgba(19, 27, 89, 0) 0%, #2F3DA0 100%)",
    filter: "drop-shadow(2px 2px 10px rgba(0, 0, 0, 0.1))",
    padding: "20px 20px 70px 20px",
    transform: "scale(1)",

    "&:hover": {
      transform: "scale(1.05)",
    },
  },

  title: {
    color: "white",
    whiteSpace: "pre-line",
    fontSize: "11px",
    fontWeight: 100,
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    lineHeight: "16px",

    [breakpoints.down("sm")]: {
      whiteSpace: "unset",
      fontSize: "11px",
    },
  },

  content: {
    color: "white",
    fontSize: "34px",
    lineHeight: "110%",
    fontWeight: 900,
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",

    [breakpoints.down("sm")]: {
      fontSize: "30px",
    },
  },

  hr: {
    margin: "20px 0px 10px 0px",
    background: "linear-gradient(-90deg, rgba(115, 214, 241, 0) -5.46%, #73D6F1 101.08%)",
    borderRadius: "10px",
    height: "3px",
  }
}));

export interface StatBoxProps {
  image?: any;
  title?: string;
  content?: string;
  custom_style?: object;
  delay?: number;
}

const StatBox: React.FC<StatBoxProps> = ({
  image,
  title,
  content,
  custom_style,
  delay = 0
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <ScrollAnimation
      delay={delay}
      animateIn="flipInY"
      animateOut="flipOutY"
      className={cx(classes.bg)}
      style={custom_style}
    >
      <Box top="-70px" position="absolute">
        <img height="130px" src={image} alt="title" />
      </Box>

      <Box mt="50px" />

      <Box className={cx(classes.content)}>{content}</Box>

      <Box className={cx(classes.hr)} />

      <Box className={cx(classes.title)}>{title}</Box>

      <Box mt="30px"></Box>
    </ScrollAnimation>
  );
};

export default StatBox;
