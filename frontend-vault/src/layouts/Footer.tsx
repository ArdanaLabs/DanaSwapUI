import React from "react";
import { Box, useMediaQuery, Container, Link } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useHistory } from "react-router-dom";

import { useIsDarkMode } from "state/user/hooks";

import DUSD_LOGO_BLUE from "assets/image/DUSD-LOGO-BLUE.png";
import DUSD_LOGO_WHITE from "assets/image/DUSD-LOGO-WHITE.png";
import BG_BLUE from "assets/image/backgrounds/BG-FOOTER-BLUE.png";
import BG_WHITE from "assets/image/backgrounds/BG-FOOTER-WHITE.png";
import { socials } from "data";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: `url(${
      palette.type === "dark" ? BG_BLUE : BG_WHITE
    }) center center no-repeat`,
    backgroundSize: "cover",
    marginTop: "50px",
    filter: "drop-shadow(35px 0px 15px rgba(0, 0, 0, 0.12))",
  },

  logo: {
    paddingLeft: "10px",
    display: "flex",
    alignItems: "center",
    cursor: "pointer",
    "& img": {
      width: "60px",

      [breakpoints.down("xs")]: {
        marginBottom: "30px",
      },
    },
  },

  container: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    transition: "background .2s ease-in",
    padding: "20px 0px",

    [breakpoints.down("xs")]: {
      flexDirection: "column",
    },
  },

  socials: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    width: "300px",

    "& > .link": {
      lineHeight: 0,
    },

    "& > .link svg": {
      width: "25px",
    },
    "& > .link path": {
      fill: palette.primary.main,
    },
  },
}));

const Footer: React.FC = () => {
  const theme = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(theme.breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  const history = useHistory();

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box className={cx(classes.container)}>
          <Box className={cx(classes.logo)} onClick={() => history.push("/")}>
            <img
              src={
                theme.palette.type === "dark" ? DUSD_LOGO_WHITE : DUSD_LOGO_BLUE
              }
              alt="DANA Logo"
            />
          </Box>
          <Box className={cx(classes.socials)}>
            {socials.map((social: any, index: number) => (
              <Link
                className="link"
                href={social.url}
                key={index}
                rel="noopener noreferrer"
                target="_blank"
              >
                <social.image />
              </Link>
            ))}
          </Box>
        </Box>
      </Container>
    </Box>
  );
};

export default Footer;
