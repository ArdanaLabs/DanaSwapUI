import React from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";

import img_Ellipse from "assets/img/landing/backgrounds/ellipse.png";

const useStyles = makeStyles(({ palette }) => ({
  bg: {
    background: `url(${img_Ellipse}) right bottom no-repeat, #FFFFFF`,
    padding: "20px"
  },
}));

const DanaswapSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
        <Grid container alignItems="center">
          <Grid item xs={12} sm={6}>
            <Box
              p="20px"
            >
              <Box>
                
              </Box>
            </Box>
          </Grid>
        </Grid>
    </Box>
  );
};

export default DanaswapSection;
