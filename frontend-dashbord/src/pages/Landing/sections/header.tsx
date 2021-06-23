import React, { useState } from "react";
import {
  useMediaQuery,
  Grid,
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

const useStyles = makeStyles(({ palette }) => ({
  bg: {
    zIndex: 1,
  },
  logo_img: {
    width: "287px",
    height: "136px",
  },
  logo_img_mobile: {
    width: "187px",
    height: "96px",
  },
  menuItem: {
    padding: "15px",
    color: "white",
    fontSize: "18px",
  },
}));

const links = [
  {
    label: "HOME",
    to: "/home",
  },
  {
    label: "RESOURCES",
    to: "/home",
  },
  {
    label: "ROADMAP",
    to: "/home",
  },
  {
    label: "BLOG",
    to: "/home",
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
    <Grid container direction="row" justify="space-between" alignItems="center">
      <Box>
        <img src={Logo} className={cx(classes.logo_img, {[classes.logo_img_mobile]: mobile})} alt="logo" />
      </Box>
      {!mobile && (
        <Box>
          {links.map((link, index) => {
            return (
              <Link
                href={link.to}
                className={cx(classes.menuItem)}
                key={index}
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
            onClick={() =>
              setOpenMenu(!openMenu)
            }
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
                  style={{color: "black"}}
                >
                  {link.label}
                </Link>
              );
            })}
          </Drawer>
        </>
      )}
    </Grid>
  );
};

export default HeaderSection;
