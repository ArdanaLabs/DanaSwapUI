import React from "react";
import { Box, Grid, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { OverViewBox } from "components/Box";

import { ovList } from "data";

const useStyles = makeStyles(({ palette }) => ({
  self: {
    // background: "transparent",
    // margin: "auto -10px",
  },
}));

const OverViewSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.self)}>
      <Grid container>
        <Grid container item sm={12} md={6}>
          {ovList
            .filter((value: any, index: any, array: any) => {
              return index < 4;
            })
            .map((ov: any, i: any) => (
              <Grid item xs={6} sm={3} key={i}>
                <OverViewBox label={ov.label} content={ov.content} />
              </Grid>
            ))}
        </Grid>
        <Grid container item sm={12} md={6}>
          {ovList
            .filter((value: any, index: any, array: any) => {
              return index >= 4;
            })
            .map((ov: any, i: any) => (
              <Grid item xs={6} sm={3} key={i}>
                <OverViewBox label={ov.label} content={ov.content} />
              </Grid>
            ))}
        </Grid>
      </Grid>
    </Box>
  );
};

export default OverViewSection;
