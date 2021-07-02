import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import ScrollAnimation from "react-animate-on-scroll";
import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {},

  name: {
    fontFamily: "Brandon Grotesque Bold",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "32px",
    lineHeight: "51px",
    color: "#2F3DA0",
    paddingTop: "10px",

    [breakpoints.down("sm")]: {
      fontSize: "24px",
    },
  },

  job: {
    fontFamily: "'Museo Sans 300'",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "18px",
    lineHeight: "22px",
    color: "#423F3F",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
    },
  },

  info: {
    fontFamily: "'Museo Sans 300'",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "18px",
    lineHeight: "22px",
    color: "#000000",
    whiteSpace: "pre-wrap",
    padding: "15px 0",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
      lineHeight: "16px",
    },
  },
}));

export interface ProfileBoxProps {
  image?: any;
  name?: string;
  job?: string;
  info?: string;
  custom_style?: object;
}

const ProfileBox: React.FC<ProfileBoxProps> = ({
  image,
  name,
  job,
  info,
  custom_style,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box position="relative" flex="2" marginY="20px" style={custom_style}>
      <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
        <Box borderRadius="30px">
          <img src={image} alt={name} width="100%" />
        </Box>

        <Box className={cx(classes.name)}>{name}</Box>

        <Box className={cx(classes.job)}>{job}</Box>

        <Box className={cx(classes.info)}>{info}</Box>
      </ScrollAnimation>
    </Box>
  );
};

export default ProfileBox;
