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
  logoImg: {
    width: "287px",
    height: "136px",
  },
  mobile_logoImg: {
    width: "187px",
    height: "96px",
  },
  menuItem: {
    padding: "15px",
    color: "white",
    fontSize: "18px",
    transition: "color .2s",

    "&:hover": {
      color: "#7A7A7A",
    }
  },
}));

const links = [
  {
    label: "HOME",
    to: "#",
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
    <Grid container direction="row" justify="space-between" alignItems="center">
      <Box>
        <img src={Logo} className={cx(!mobile ? classes.logoImg : classes.mobile_logoImg)} alt="logo" />
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
