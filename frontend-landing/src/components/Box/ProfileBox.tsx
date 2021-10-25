import React, { useState } from "react";
import cx from "classnames";
import { Box, Link, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import { Avatar } from "components/Avatar";

import ICO_LINKEDIN from "assets/icons/linkedin.svg";
import ICO_TWITTER from "assets/icons/twitter.svg";
import ICO_GITHUB from "assets/icons/github.svg";
import ICO_EXPAND from "assets/icons/expand.svg";
import ICO_COLLAPSE from "assets/icons/collapse.svg";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
    background: palette.background.paper,
    borderRadius: "10px",
    padding: "90px 20px 50px",
    textAlign: "center",
    marginTop: "80px",
    height: "400px",

    [breakpoints.down("xs")]: {
      padding: "50px 10px 40px",
      "&.collapsed": {
        height: "180px",
      },
    },
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
    fontSize: "25px",
    lineHeight: "110%",
    color: palette.text.primary,
    marginBottom: "5px",

    [breakpoints.down("xs")]: {
      fontSize: "18px",
    },
  },

  role: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 600,
    fontSize: "19px",
    lineHeight: "110%",
    color: palette.text.secondary,
    marginBottom: "20px",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
    },
  },

  info: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontSize: "17px",
    lineHeight: "110%",
    color: palette.text.primary,
    whiteSpace: "pre-line",
    opacity: 1,

    "& > strong": {
      fontWeight: 900,
    },

    "&.hide": {
      opacity: 0,
    },

    [breakpoints.down("sm")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
    },
  },

  socials: {
    position: "absolute",
    width: "50%",
    bottom: "50px",
    left: "50%",
    transform: "translate(-50%, 0%)",
    display: "flex",
    alignItems: "center",
    justifyContent: "space-around",
    visibility: "visible",
    opacity: 1,

    "& > a": {
      display: "flex",
      "& > img": {
        width: "25px",
        maxWidth: "max-content",
      },

      "&:hover": {
        opacity: 0.8,
      },
    },

    [breakpoints.down("xs")]: {
      bottom: "40px",
      width: "80%",

      "&.hide": {
        visibility: "hidden",
        opacity: 0,
      },
    },
  },

  expandButton: {
    position: "absolute",
    left: "50%",
    bottom: "0",
    transform: "translate(-50%, 50%)",
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
    twitter?: string;
  };
}

export interface ProfileBoxProps {
  profile: ProfileType;
}

const ProfileBox: React.FC<ProfileBoxProps> = ({
  profile: { avatar, name, role, info, socials },
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  const [expand, setExpand] = useState<boolean>(false);

  const handleExpand = () => {
    setExpand(!expand);
  };

  return (
    <Box className={cx(classes.root, { collapsed: mobile && !expand })}>
      <Box className={cx(classes.image)}>
        <Avatar image={avatar} size={100} />
      </Box>

      <Box className={cx(classes.name)}>{name}</Box>
      <Box className={cx(classes.role)}>{role}</Box>

      <Box
        className={cx(classes.info, { hide: mobile && !expand })}
        dangerouslySetInnerHTML={{
          __html: info,
        }}
      />

      <Box className={cx(classes.socials, { hide: mobile && !expand })}>
        {socials.linkedin && (
          <Link href={socials.linkedin} target="_blank">
            <img src={ICO_LINKEDIN} alt="linkedin" />
          </Link>
        )}
        {socials.twitter && (
          <Link href={socials.twitter} target="_blank">
            <img src={ICO_TWITTER} alt="twitter" />
          </Link>
        )}
        {socials.github && (
          <Link href={socials.github} target="_blank">
            <img src={ICO_GITHUB} alt="github" />
          </Link>
        )}
      </Box>

      {mobile && (
        <Box className={cx(classes.expandButton)} onClick={handleExpand}>
          <img src={expand ? ICO_COLLAPSE : ICO_EXPAND} alt="expand" />
        </Box>
      )}
    </Box>
  );
};

export default ProfileBox;
