import React from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
    "& video": {
      objectFit: "cover",
    },
  },
}));

const MapSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return <Box className={cx(classes.root)}></Box>;
};

export default MapSection;
