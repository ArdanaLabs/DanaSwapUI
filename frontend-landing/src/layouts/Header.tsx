import React, { useState } from "react";
import {
  useMediaQuery,
  Box,
  Link,
  Drawer,
  IconButton,
  Container,
} from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import Hamburger from "hamburger-react";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";

import LOGO_White from "assets/logo_white.png";
import LOGO_Text from "assets/logo_text.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "absolute",
    left: 0,
    top: 0,
    width: "100%",
    background: "transparent",
    zIndex: 1,
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
      padding: "20px 5px",
      "&:first-child": {
        width: "55px",
      },
      "&:last-child": {
        height: "55px",
      },
    },
  },
  drawer: {
    padding: "10px",
  },
  menuItem: {
    fontFamily: "Museo Sans",
    fontWeight: 900,
    fontStyle: "normal",
    lineHeight: "100%",
    margin: "auto 20px",
    padding: "8px 0px",
    color: "white",
    fontSize: "13px",
    position: "relative",
    textAlign: "center",

    "&:hover": {
      color: "#73D6F1",
    },

    "&.active": {
      color: palette.text.secondary,
      "&::before": {
        content: "' '",
        position: "absolute",
        top: "100%",
        width: "100%",
        left: 0,
        height: "2.5px",
        borderRadius: "2px",
        background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",

        [breakpoints.down("xs")]: {
          display: "none",
        },
      },
    },
  },
}));

const links = [
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.0"),
    to: "/",
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.1"),
    to: "/team",
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.2"),
    to: "#",
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.3"),
    to: "https://faceted-wash-97d.notion.site/cb0d147034e6439f8e70b2698ce199f2?v=fbf6185ab5f143eb9e22064fd9647814",
    blank: true,
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.4"),
    to: "https://docs.ardana.org/?_ga=2.267695815.895783086.1634911974-1488848088.1632832057",
    blank: true,
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.5"),
    to: "https://medium.com/ardana-hub",
    blank: true,
  },
];

const HeaderSection: React.FC = () => {
  const { palette, breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const [openMenu, setOpenMenu] = useState(false);

  const toggleMenu = () => {
    setOpenMenu((prev) => !prev);
  };

  const activeMenu = (to: string): boolean => {
    const path = "/" + window.location.pathname.split("/").pop();
    if (to === path) {
      return true;
    }
    return false;
  };

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box className={cx(classes.self)}>
          <Box className={cx(classes.logo)}>
            <img src={LOGO_White} alt="logo" />
            <img src={LOGO_Text} alt="logo" />
          </Box>
          {!mobile && (
            <Box>
              {links.map((link, index) => {
                return (
                  <Link
                    href={link.to}
                    className={cx(classes.menuItem, {
                      active: activeMenu(link.to),
                    })}
                    key={index}
                    underline="none"
                    rel="noopener noreferrer"
                    target={link.blank ? "_blank" : "_self"}
                  >
                    {link.label}
                  </Link>
                );
              })}
            </Box>
          )}
          {mobile && (
            <>
              <IconButton
                style={{ height: "48px", padding: 0 }}
                onClick={() => setOpenMenu(!openMenu)}
              >
                <Hamburger
                  size={24}
                  distance={"lg"}
                  color={palette.common.white}
                  toggled={openMenu}
                  toggle={setOpenMenu}
                />
              </IconButton>
              <Drawer
                className={cx(classes.drawer)}
                anchor={"top"}
                open={openMenu}
                onClose={toggleMenu}
              >
                {links.map((link, index) => (
                  <Link
                    key={index}
                    className={cx(classes.menuItem)}
                    href={link.to}
                    underline="none"
                    rel="noopener noreferrer"
                    target={link.blank ? "_blank" : "_self"}
                  >
                    {link.label}
                  </Link>
                ))}
              </Drawer>
            </>
          )}
        </Box>
      </Container>
    </Box>
  );
};

export default HeaderSection;
