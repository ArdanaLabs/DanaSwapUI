import React from "react";
import cx from "classnames";
import { Box, Link, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import { GradientButton } from "components";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    width: "100%",
  },
  background: {
    "& > img": {
      borderRadius: "10px",
    },
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontWeight: 700,
    fontSize: "35px",
    lineHeight: "100%",
    color: palette.common.white,
    whiteSpace: "pre-line",

    [breakpoints.down("xs")]: {
      fontSize: "30px",
    },
  },
}));

interface TechnicalPaperBoxProps {
  image: string;
  title: string;
  link: string;
}

const TechnicalPaperBox: React.FC<TechnicalPaperBoxProps> = ({
  image,
  title,
  link,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)} position="relative">
      <Box className={cx(classes.background)}>
        <img src={image} alt="background" width="100%" />
      </Box>
      <Box position="absolute" bottom="30px" left="30px" textAlign="left">
        <Box className={cx(classes.title)}>{title}</Box>
        <Box mt="20px" />
        <Box>
          <Link target="_blank" underline="none" href={link}>
            <GradientButton label="LEARN MORE" width={145} height={40} />
          </Link>
        </Box>
      </Box>
    </Box>
  );
};

export default TechnicalPaperBox;
