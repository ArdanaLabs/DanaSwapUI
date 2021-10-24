import React from "react";
import { Box, useMediaQuery, Link, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import _ from "lodash";

import { useIsDarkMode } from "state/user/hooks";

import { externals, socials } from "data";
import BG_WAVE from "assets/backgrounds/wave-gradient.png";
import BG_WAVE_MOBILE from "assets/backgrounds/wave-mobile180-bg.png";
import BG_RING from "assets/backgrounds/ring.svg";
import LOGO_BLUE from "assets/logo_blue.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: `url(${BG_WAVE}) top left no-repeat`,
    backgroundSize: "100%",
    padding: "150px 0px 60px",
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
        fontSize: "16px",
        lineHeight: "19px",
      },
    },

    [breakpoints.down("xs")]: {
      flexDirection: "column",
      paddingLeft: "20px",
      paddingRight: "20px",
      textAlign: "center",
      alignItems: "center",
      background: `url(${BG_WAVE_MOBILE}) top -30px left no-repeat`,
      backgroundSize: "100%",
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
    [breakpoints.down("xs")]: {
      fontSize: "25px",
      lineHeight: "27.5px",
    },
  },

  content: {
    color: palette.text.primary,
    fontFamily: "Museo Sans",
    fontSize: "22px",
    lineHeight: "25px",
    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
    },
  },

  logo: {
    display: "inline-flex",
    justifyContent: "center",
    alignItems: "center",
    width: "250px",
    height: "250px",
    padding: "40px",
    borderRadius: "50%",
    background: `url(${BG_RING}) center center no-repeat`,
    backgroundSize: "cover",

    "& > img": {
      borderRadius: "50%",
      [breakpoints.down("xs")]: {
        width: "60px",
      },
    },

    [breakpoints.down("xs")]: {
      width: "150px",
      height: "150px",
    },
  },

  guide: {
    width: "100%",
    display: "flex",
    justifyContent: "space-between",
    textAlign: "left",

    "& > .section": {
      display: "flex",
      flexDirection: "column",
      marginBottom: "20px",
    },

    [breakpoints.down("xs")]: {
      flexDirection: "column",
      textAlign: "center",
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

      "& > .link:hover path": {
        fill: palette.text.secondary,
      },

      "& img": {
        width: "25px",
      },
      [breakpoints.down("xs")]: {
        width: "100%",
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
      <Container maxWidth="md">
        <Box className={cx(classes.container)}>
          <Box className={cx(classes.title)}>
            Ardana is the leading cross-chain stablecoin and DEX available on
            Cardano.
          </Box>
          <Box mt={!mobile ? "30px" : "10px"} />
          <Box className={cx(classes.content)}>
            Our mission is to create opportunity and transparency for all with
            groundbreaking DeFi protocols. Our DeFi protocols cross layers and
            enable individuals worldwide to contribute their talents and skills
            to a new global economy. We want to support this vision and help
            developers build the new coordination mechanisms of the Internet age
            as we move toward a radically new vision of the future of work.
          </Box>
          <Box my={"30px"} className={cx(classes.logo)}>
            <img src={LOGO_BLUE} alt="logo" />
          </Box>
          <Box className={cx(classes.guide)}>
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
            <Box className={cx(classes.socials)}>
              <span>Our Socials</span>
              <span>
                Follow us to hear about
                <br />
                Ardana news and events
              </span>

              <Box mt={!mobile ? "70px" : "25px"} />
              <Box className="link-container">
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
          </Box>
        </Box>
      </Container>
    </Box>
  );
};

export default Footer;
