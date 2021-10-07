import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import { Avatar } from "components/Avatar";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
    background: palette.background.paper,
    borderRadius: "10px",
    padding: "10px",
  },

  image: {
    position: "absolute",
    top: "-10px",
    left: "50%",
    transform: "translate(-50%, -50%)",
  },

  name: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "30px",
    lineHeight: "110%",
    color: palette.text.primary,

    [breakpoints.down("sm")]: {
      fontSize: "24px",
    },
  },

  role: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 600,
    fontSize: "23px",
    lineHeight: "110%",
    color: palette.text.secondary,

    [breakpoints.down("sm")]: {
      fontSize: "16px",
    },
  },

  info: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontSize: "21px",
    lineHeight: "25px",
    color: palette.text.primary,
    whiteSpace: "pre-line",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
      lineHeight: "16px",
    },
  },
}));

export interface ProfileType {
  avatar: string;
  name: string;
  role: string;
  info: string;
  socials: {
    github?: string;
    linkedin?: string;
  };
}

export interface ProfileBoxProps {
  profile: ProfileType;
}

const ProfileBox: React.FC<ProfileBoxProps> = ({
  profile: { avatar, name, role, info },
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <Box className={cx(classes.image)}>
        <Avatar image={avatar} size={100} />
      </Box>

      <Box className={cx(classes.name)}>{name}</Box>

      <Box className={cx(classes.role)}>{role}</Box>

      <Box className={cx(classes.info)}>{info}</Box>
    </Box>
  );
};

export default ProfileBox;
