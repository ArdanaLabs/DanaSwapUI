import React, { useState } from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import { GradientButton } from "components/Button";

export interface DanaswapFeatureProps {
  image?: any;
  title?: string;
  content?: string;
  custom_style?: object;
}

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
    padding: "30px 30px 30px 30px",
    textAlign: "center",
    cursor: "pointer",
    width: "400px",
    height: "500px",
    background: palette.background.paper,

    [breakpoints.down("xs")]: {
      width: "100%",
      height: "230px",
      marginTop: "80px",
      paddingTop: "80px",
    },
  },

  image: {
    position: "relative",
    lineHeight: 0,
    paddingTop: "20px",
    paddingBottom: "20px",

    [breakpoints.down("xs")]: {
      position: "absolute",
      top: "-10px",
      left: "50%",
      transform: "translate(-50%, -50%)",
    },
  },
  photo: {
    position: "absolute",
    top: "50%",
    left: "50%",
    transform: "translate(-50%, -50%)",
    display: "inline-flex",
    justifyContent: "center",
    alignItems: "center",
    width: "80px",
    [breakpoints.down("xs")]: {
      width: "60px",
    },
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontWeight: 900,
    fontSize: "30px",
    lineHeight: "110%",
    color: palette.text.primary,

    [breakpoints.down("xs")]: {
      fontSize: "25px",
      lineHeight: "27.5px",
    },
  },

  content: {
    fontFamily: "Museo Sans",
    fontSize: "20px",
    lineHeight: "26px",
    color: palette.text.primary,

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "19.2px",
    },
  },

  border: {
    position: "absolute",
    bottom: "0px",
    left: "0px",
    background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
    boxShadow: "0px 0px 20px 5px #2D3BA0",
    borderRadius: "7.5px 7.5px 0px 0px",
    width: "100%",
    height: "15px",
    opacity: 0,

    "&.hover": {
      opacity: 1,
    },
  },
}));

const DanaswapFeature: React.FC<DanaswapFeatureProps> = ({
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
    <Box
      className={cx(classes.root)}
      onMouseEnter={() => setHover(true)}
      onMouseLeave={() => setHover(false)}
    >
      <Box className={cx(classes.image)}>
        <GradientButton
          width={!mobile ? 121 : 81}
          height={!mobile ? 121 : 81}
          clickable={false}
        />
        <img className={cx(classes.photo)} src={image} alt="" />
      </Box>
      <Box className={cx(classes.title)}>{title}</Box>
      <Box mt={!mobile ? "30px" : "15px"} />
      <Box className={cx(classes.content)}>{content}</Box>
      <Box className={cx(classes.border, { hover: hover })} />
    </Box>
  );
};

export default DanaswapFeature;
