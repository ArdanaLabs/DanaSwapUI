import React, { useState } from "react";
import {
  useMediaQuery,
  Box,
  Link,
  Drawer,
  IconButton,
} from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import Hamburger from "hamburger-react";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";

import LOGO_White from "assets/logo_white.png";
import LOGO_Text from "assets/logo_text.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
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
      },
    },
  },
}));

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
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.4"),
    to: "#",
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

  return (
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
                className={cx(classes.menuItem, { active: index === 0 })}
                key={index}
                underline="none"
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
            {links.map((link, index) => {
              return (
                <Link
                  href={link.to}
                  className={cx(classes.menuItem, "active")}
                  key={index}
                  style={{
                    textAlign: "center",
                  }}
                >
                  {link.label}
                </Link>
              );
            })}
          </Drawer>
        </>
      )}
    </Box>
  );
};

export default HeaderSection;
