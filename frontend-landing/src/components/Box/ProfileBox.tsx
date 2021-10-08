import React from "react";
import cx from "classnames";
import { Box, Link, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import { Avatar } from "components/Avatar";

import ICO_LINKEDIN from "assets/icons/linkedin.svg";
import ICO_TWITTER from "assets/icons/twitter.svg";
import ICO_GITHUB from "assets/icons/github.svg";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
    background: palette.background.paper,
    borderRadius: "10px",
    padding: "75px 20px 50px",
    textAlign: "center",
    marginTop: "70px",
    height: "400px",
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

    [breakpoints.down("sm")]: {
      fontSize: "24px",
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

    "& > strong": {
      fontWeight: 900,
    },

    [breakpoints.down("sm")]: {
      fontSize: "16px",
      lineHeight: "16px",
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

    "& > a": {
      display: "flex",
      "& > img": {
        width: "25px",
        maxWidth: "max-content",
      },

      '&:hover': {
        opacity: 0.8,
      }
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

  return (
    <Box className={cx(classes.root)}>
      <Box className={cx(classes.image)}>
        <Avatar image={avatar} size={100} />
      </Box>

      <Box className={cx(classes.name)}>{name}</Box>

      <Box className={cx(classes.role)}>{role}</Box>

      <Box
        className={cx(classes.info)}
        dangerouslySetInnerHTML={{
          __html: info,
        }}
      />

      <Box className={cx(classes.socials)}>
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
    </Box>
  );
};

export default ProfileBox;
