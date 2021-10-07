import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

export interface AvatarProps {
  image: string;
  size: number;
}

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    boxShadow: "0px 0px 20px 5px #2D3BA0",
    padding: "10px",
    borderRadius: "50%",
    display: "inline-flex",
    justifyContent: "center",
    alignItems: "center",
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
        width={size + "px"}
        height={size + "px"}
      />
    </Box>
  );
};

export default Avatar;
