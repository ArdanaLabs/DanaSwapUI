import React from "react";
import cx from "classnames";
import {
  Box,
  Container,
  makeStyles,
  useMediaQuery,
  useTheme,
} from "@material-ui/core";
import { useIsDarkMode } from "state/user/hooks";
import Header from "./Header";

import { navList } from "data";
import LOGO_LIGHT from "assets/Ardana_hor_light.svg";
import LOGO_DARK from "assets/Ardana_hor_dark.svg";
import Footer from "./Footer";

const useStyles = makeStyles(({ palette }) => ({
  self: {
    background: palette.background.default,
    paddingTop: "210px",
  },
}));

export interface LayoutProps {
  children: any;
}

const Layout: React.FC<LayoutProps> = ({ children }) => {
  const dark = useIsDarkMode();
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.self)}>
      <Header navList={navList} logo={!dark ? LOGO_LIGHT : LOGO_DARK} />
      <Container>{children}</Container>
      <Footer />
    </Box>
  );
};

export default Layout;
