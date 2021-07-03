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
import { withTranslation, WithTranslation } from "react-i18next";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";

import LOGO_Blue from "assets/logo_blue.png";
import LOGO_Text from "assets/logo_text.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  self: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
  },
  logo: {
    paddingLeft: "10px",
    display: "flex",
    alignItems: "center",
    cursor: "pointer",
    "& img": {
      padding: "20px 10px",
    },
  },
  menuItem: {
    fontFamily: "Brandon Grotesque Bold",
    fontStyle: "normal",
    lineHeight: "100%",
    padding: "15px",
    color: "white",
    fontSize: "18px",
    transition: "all .2s",

    "&:hover": {
      color: "#7A7A7A",
    },
  },
}));

const links = [
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.0"),
    to: "/home",
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

const HeaderSection: React.FC<WithTranslation> = ({ t }) => {
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
        <img src={LOGO_Blue} alt="logo" />
        <img src={LOGO_Text} alt="logo" />
      </Box>
      {!mobile && (
        <Box>
          {links.map((link, index) => {
            return (
              <Link
                href={link.to}
                className={cx(classes.menuItem)}
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
          <Drawer anchor={"top"} open={openMenu} onClose={toggleMenu}>
            {links.map((link, index) => {
              return (
                <Link
                  href={link.to}
                  className={cx(classes.menuItem)}
                  key={index}
                  style={{
                    textAlign: "center",
                    background:
                      "linear-gradient(90.19deg, #2F3DA0 27.19%, #73D6F1 99.87%)",
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

export default withTranslation()(HeaderSection);
