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

import { useIsDarkMode } from "state/user/hooks";

import Logo from "assets/img/landing/logos/ardana-hor.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  self: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
  },
  logoImg: {
    width: "287px",
    height: "136px",

    [breakpoints.down("xs")]: {
      width: "187px",
      height: "96px",
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
    label: "HOME",
    to: "/home",
  },
  {
    label: "RESOURCES",
    to: "#",
  },
  {
    label: "ROADMAP",
    to: "#",
  },
  {
    label: "BLOG",
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
      <img src={Logo} className={cx(classes.logoImg)} alt="logo" />
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
                  style={{ color: "black" }}
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
