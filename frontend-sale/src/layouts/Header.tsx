import React from "react";
import { Box, useMediaQuery, Container, Link } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";
import LOGO_White from "assets/logo_white.png";
import LOGO_Text from "assets/logo_text.png";

const links = [
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.0"),
    to: "#",
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.1"),
    to: "#",
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.2"),
    to: "#",
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.3"),
    to: "#",
  },
];

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: "transparent",
    position: "absolute",
    top: "0px",
    width: "100%",
  },
  self: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    paddingTop: "10px",
  },
  logo: {
    paddingLeft: "10px",
    display: "flex",
    alignItems: "center",
    cursor: "pointer",
    "& img": {
      padding: "20px 6px",
      "&:first-child": {
        width: "80px",
      },
      "&:last-child": {
        height: "65px",
      },
    },
  },
  menuItem: {
    fontFamily: "Museo Sans",
    fontWeight: 900,
    fontStyle: "normal",
    lineHeight: "100%",
    margin: "auto 20px",
    padding: "8px 0px",
    color: "white",
    fontSize: "14px",
    position: "relative",

    "&:hover": {
      color: "#73D6F1",
    },

    "&.active": {
      color: "#73D6F1",
      "&::before": {
        content: "' '",
        position: "absolute",
        top: "100%",
        width: "100%",
        left: 0,
        height: "2.5px",
        borderRadius: "2px",
        background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
      },
    },
  },
}));

const Header: React.FC = () => {
  const theme = useTheme();
  const mobile = useMediaQuery(theme.breakpoints.down("sm"));
  const dark = useIsDarkMode();
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box className={cx(classes.self)}>
          <Box className={cx(classes.logo)}>
            <img src={LOGO_White} alt="logo" />
            <img src={LOGO_Text} alt="logo" />
          </Box>

          <Box>
            {links.map((link, index) => {
              return (
                <Link
                  href={link.to}
                  className={cx(classes.menuItem, { active: index === 0 })}
                  key={index}
                  underline="none"
                >
                  {link.label}
                </Link>
              );
            })}
          </Box>
        </Box>
      </Container>
    </Box>
  );
};

export default Header;
