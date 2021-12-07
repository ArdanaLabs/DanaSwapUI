import React from "react";
import cx from "classnames";
import {
  Box,
  makeStyles,
  useMediaQuery,
  useTheme,
} from "@material-ui/core";
import { useIsDarkMode } from "state/user/hooks";
import { Footer, Header } from "layouts";

const useStyles = makeStyles(({ palette }) => ({
  root: {
    background: palette.background.default,
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
    <Box className={cx(classes.root)}>
      <Header />
      <Box>{children}</Box>
      <Footer />
    </Box>
  );
};

export default Layout;
