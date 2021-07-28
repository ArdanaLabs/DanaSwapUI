import React from "react";
import { Box, Grid, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { OverViewBox } from "components/Box";

import { useTotalStats } from "state/home/hooks";
import { nFormatter } from "hooks";

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

  const { totalDepositsAllPoolsUSD, totalDailyVolumeUSD, totalDailyFeeVolumeUSD } = useTotalStats();
  
  return (
    <Box className={cx(classes.self)}>
      <Grid container>
        <Grid container item sm={12} md={6}>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TVL"} content={"$220.21 M"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TOTAL LIQUIDITY"} content={nFormatter(totalDepositsAllPoolsUSD, 2)} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TOTAL BOND"} content={(totalDailyVolumeUSD && totalDepositsAllPoolsUSD) ? ((100 * (totalDailyVolumeUSD / totalDepositsAllPoolsUSD)).toFixed(2) + "%") : "0%"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"FUNDS CAP OVERVIEW"} content={"100.6%"} />
          </Grid>
        </Grid>
        <Grid container item sm={12} md={6}>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TOTAL VOLUME"} content={nFormatter(totalDailyVolumeUSD, 2)} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"24H VOLUME"} content={nFormatter(totalDailyVolumeUSD, 2)} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TOTAL POOLED"} content={nFormatter(totalDailyFeeVolumeUSD, 2)} />
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
