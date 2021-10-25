import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

import { GradientButton } from "components/Button";
export interface AvatarProps {
  image: string;
  size?: number;
}

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
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
}));

const Avatar: React.FC<AvatarProps> = ({ image, size }) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <GradientButton
        width={!mobile ? 145 : 81}
        height={!mobile ? 145 : 81}
        clickable={false}
      />
      <img className={cx(classes.photo)} src={image} alt="avatar" />
    </Box>
  );
};

export default Avatar;
