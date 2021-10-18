import React from "react";
import { Box, useMediaQuery, Link, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import _ from "lodash";

import { useIsDarkMode } from "state/user/hooks";

import { externals, socials } from "data";
import BG_WAVE from "assets/backgrounds/wave.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: ` linear-gradient(0deg, #080E42 32.04%, rgba(8, 14, 66, 0) 95.34%) top left no-repeat, 
                  url(${BG_WAVE}) top left no-repeat`,
    backgroundSize: "100%",
    padding: "100px 0px",
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
      "& > a": {
        color: palette.common.white,
      },
      fontFamily: "Museo Sans",
      fontSize: "20px",
      lineHeight: "30px",
      cursor: "pointer",

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

  container: {
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
    flexDirection: "column",
    textAlign: "center",
  },

  title: {
    color: palette.text.secondary,
    fontFamily: "Brandon Grotesque",
    fontWeight: 900,
    fontSize: "40px",
    lineHeight: "100%",
  },

  content: {
    color: palette.text.primary,
    fontFamily: "Museo Sans",
    fontSize: "22px",
    lineHeight: "25px",
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
      <Container maxWidth="sm">
        <Box className={cx(classes.container)}>
          <Box className={cx(classes.title)}>
            Ardana is the leading cross-chain stablecoin and DEX available on
            Cardano.
          </Box>
          <Box mt="30px" />
          <Box className={cx(classes.content)}>
            Our mission is to create opportunity and transparency for all with
            groundbreaking DeFi protocols. Our DeFi protocols cross layers and
            enable individuals worldwide to contribute their talents and skills
            to a new global economy. We want to support this vision and help
            developers build the new coordination mechanisms of the Internet age
            as we move toward a radically new vision of the future of work.
          </Box>
        </Box>
      </Container>
      {/* <Box className={cx(classes.guide)}>
        <Box>
          <Box className={cx(classes.logo)}>
            <img src={LOGO_White} alt="logo" />
            <img src={LOGO_Text} alt="logo" />
          </Box>
        </Box>

        {_.keys(externals).map((group) => (
          <Box className="section" key={group}>
            <span>{group}</span>
            {_.keys(externals[group]).map((external) => (
              <span key={external}>
                <Link
                  rel="noopener noreferrer"
                  target="_blank"
                  href={externals[group][external]}
                >
                  {external}
                </Link>
              </span>
            ))}
          </Box>
        ))}
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
          {socials.map((social: any, index: number) => (
            <Link
              className="link"
              href={social.url}
              key={index}
              rel="noopener noreferrer"
              target="_blank"
            >
              <img src={social.image} alt="social Link" />
            </Link>
          ))}
        </Box>
      </Box> */}
    </Box>
  );
};

export default Footer;
