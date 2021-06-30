import React from "react";
import { Box, Container, Grid, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { OverViewBox } from "components/Box";

const useStyles = makeStyles(({ palette }) => ({
  self: {
    // background: "transparent",
    // margin: "auto -10px",
  },
}));

const OverViewSection: React.FC = () => {
  const { palette, breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.self)}>
      <Grid container>

        <Grid container item sm={12} md={6}>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TVL"} content={"$220.21 M"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TOTAL LIQUIDITY"} content={"$84.60 M"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TOTAL BOND"} content={"$135.61 M (22.3 M R)"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"FUNDS CAP OVERVIEW"} content={"100.6%"} />
          </Grid>
        </Grid>
        <Grid container item sm={12} md={6}>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TOTAL VOLUME"} content={"$2.37 B"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"24H VOLUME"} content={"$30.24 M"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TOTAL POOLED"} content={"$135.61 M (22.3 M R)"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"LIQUIDITY APY"} content={"30.11%"} />
          </Grid>
        </Grid>
      </Grid>
    </Box>
  );
};

export default OverViewSection;
