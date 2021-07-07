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

    "&:hover": {
      transform: "scale(1.05)",
    },
  },

  title: {
    color: "white",
    whiteSpace: "pre-line",
    fontSize: "36px",
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
    color: "white",
    fontSize: "18px",
    lineHeight: "22px",
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
    <ScrollAnimation animateIn="fadeInUp" animateOnce={true} className={cx(classes.bg)} style={custom_style}>
      <Box top="-50px" position="absolute">
        <img width={!mobile ? "170px" : "100px"} src={image} alt="title" />
      </Box>

      <Box mt={!mobile ? "100px": "50px"} />

      <Box className={cx(classes.title)}>{title}</Box>

      <Box mt="30px"></Box>

      <Box className={cx(classes.content)}>{content}</Box>

      <Box mt="30px"></Box>
    </ScrollAnimation>
  );
};

export default FeatureBox;
