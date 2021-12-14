import React from "react"
import { Box, Grid, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { OverViewBox } from "components/Box"

import { useTotalStats } from "state/home/hooks"
import { printCurrencyUSD } from "hooks"

const useStyles = makeStyles(({ palette }) => ({
  self: {
    // background: "transparent",
    // margin: "auto -10px",
  },
}))

const OverViewSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const {
    totalDepositsAllPoolsUSD,
    totalDailyVolumeUSD,
    totalDailyFeeVolumeUSD,
    totalLiquidityUtilization,
  } = useTotalStats()

  return (
    <Box className={cx(classes.self)}>
      <Grid container>
        <Grid container item sm={12} md={6}>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"TVL\n\n"} content={"$220.21M"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox
              label={"TOTAL LIQUIDITY\n\n"}
              content={printCurrencyUSD(totalDepositsAllPoolsUSD, 2)}
              // content={new Intl.NumberFormat(undefined, {
              //   currency: "USD",
              //   currencyDisplay: "narrowSymbol",
              //   style: "currency",
              //   notation: "compact",
              //   minimumFractionDigits: 2,
              // }).format(totalDepositsAllPoolsUSD ?? 0)}
            />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox
              label={"LIQUIDITY UTILIZATION"}
              content={`${totalLiquidityUtilization?.toFixed(0) ?? 0}%`}
            />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox
              label={"24H VOLUME\n\n"}
              content={printCurrencyUSD(totalDailyVolumeUSD?.trade ?? 0, 2)}
            />
          </Grid>
        </Grid>
        <Grid container item sm={12} md={6}>
          <Grid item xs={6} sm={3}>
            <OverViewBox
              label={"24H FEE GENERAGED"}
              content={printCurrencyUSD(totalDailyFeeVolumeUSD, 2)}
            />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"DANA PRICE\n\n"} content={"$220.21M"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"% OF DANA LOCKED"} content={"30.11%"} />
          </Grid>
          <Grid item xs={6} sm={3}>
            <OverViewBox label={"DANA STAKING\nAPY"} content={"30.11%"} />
          </Grid>
        </Grid>
      </Grid>
    </Box>
  )
}

export default OverViewSection
