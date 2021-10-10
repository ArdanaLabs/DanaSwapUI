import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

import BG_RING from "assets/backgrounds/ring.svg";
export interface AvatarProps {
  image: string;
  size: number;
}

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    // boxShadow: "0px 0px 20px 5px #2D3BA0",
    padding: "40px",
    borderRadius: "50%",
    display: "inline-flex",
    justifyContent: "center",
    alignItems: "center",
    // border: "3px solid #73D6F1",
    background: `url(${BG_RING}) center center no-repeat`,
    backgroundSize: 'cover',
    width: '200px',
    height: '200px',
  },

  photo: {
    borderRadius: "50%",
  },
}));

const Avatar: React.FC<AvatarProps> = ({ image, size }) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <img
        className={cx(classes.photo)}
        src={image}
        alt="avatar"
      />
    </Box>
  );
};

export default Avatar;
