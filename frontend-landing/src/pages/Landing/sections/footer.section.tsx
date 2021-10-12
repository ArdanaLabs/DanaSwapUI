import React from "react";
import { Box, useMediaQuery, Link } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";

import LOGO_White from "assets/logo_white.png";
import LOGO_Text from "assets/logo_text.png";

const socialLinks = [
  require("assets/icons/twitter.svg").default,
  require("assets/icons/discord.svg").default,
  require("assets/icons/telegram.svg").default,
  require("assets/icons/medium.svg").default,
  require("assets/icons/monkey.svg").default,
  require("assets/icons/linkedin.svg").default,
];

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: "#293599",
    padding: "60px 100px",
    display: "flex",
    justifyContent: "space-between",
    alignItems: "flex-start",

    "& span": {
      "&:first-child": {
        fontFamily: "Brandon Grotesque",
        fontSize: "25px",
        lineHeight: "36px",
        color: palette.text.secondary,
        marginBottom: "10px",
        fontWeight: 900,

        [breakpoints.down("xs")]: {
          fontSize: "20px",
          marginBottom: "0px",
        },
      },
      fontFamily: "Museo Sans",
      fontSize: "20px",
      lineHeight: "30px",
      color: palette.common.white,

      [breakpoints.down("xs")]: {
        fontSize: "15px",
      },
    },

    [breakpoints.down("xs")]: {
      flexDirection: "column",
      padding: "40px 20px",
      textAlign: "center",
      alignItems: "center",
    },
  },

  logo: {
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
    cursor: "pointer",
    marginBottom: "20px",

    "& > img": {
      "&:first-child": {
        height: "40px",
      },
      "&:last-child": {
        marginLeft: "10px",
        height: "18px",
      },
    },
  },

  guide: {
    width: "50%",
    display: "flex",
    justifyContent: "space-between",

    "& > .section": {
      display: "flex",
      flexDirection: "column",
      marginBottom: "20px",
    },

    [breakpoints.down("xs")]: {
      flexDirection: "column",
    },
  },

  socials: {
    display: "flex",
    flexDirection: "column",
    "& > .link-container": {
      display: "flex",
      justifyContent: "space-between",
      alignItems: "center",
      width: "300px",

      "& img": {
        width: "25px",
      },
    },
  },
}));

const Footer: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <Box className={cx(classes.guide)}>
        <Box>
          <Box className={cx(classes.logo)}>
            <img src={LOGO_White} alt="logo" />
            <img src={LOGO_Text} alt="logo" />
          </Box>
        </Box>
        <Box className="section">
          <span>Resources</span>
          <span>Whitepaper</span>
          <span>Pitch Deck</span>
          <span>Roadmap</span>
          <span>Docs</span>
        </Box>

        <Box className="section">
          <span>Products</span>
          <span>DEX</span>
          <span>Stablecoin</span>
          <span>Multisig</span>
        </Box>

        <Box className="section">
          <span>Company</span>
          <span>Conact Us</span>
          <span>Careers</span>
        </Box>
      </Box>
      <Box className={cx(classes.socials)}>
        <span>Our Socials</span>
        <span>
          Follow us to hear about
          <br />
          Ardana news and events
        </span>

        <Box mt="25px" />
        <Box className="link-container">
          {socialLinks.map((socialLink: string, index: number) => (
            <Link className="link" href="#" key={index}>
              <img src={socialLink} alt="social Link" />
            </Link>
          ))}
        </Box>
      </Box>
    </Box>
  );
};

export default Footer;
