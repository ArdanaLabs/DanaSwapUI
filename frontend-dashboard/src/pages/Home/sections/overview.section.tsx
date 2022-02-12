import React from "react"
import { Box, Grid, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { OverViewBox } from "components/Box"

import * as O from "fp-ts/Option"
import { RemoteData } from "fp-ts-remote-data"

import { ByTxType } from "Data/ByTxType"
import { FetchDecodeError } from "Data/FetchDecode"
import {
  TotalDeposits,
  TotalLiquidityUtilization,
  TotalDailyVolume,
  TotalDailyFeeVolume,
} from "Data/Stats/AggregateStats"
import { TotalStats } from "Data/Stats/CombinedStats"

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

  const totalStats: RemoteData<FetchDecodeError, TotalStats> = useTotalStats()

  // TODO: text-transform: uppercase

  function renderTotalDepositsAllPoolsUSD(
    totalDeposits: O.Option<TotalDeposits.Type>
  ) {
    return (
      <OverViewBox
        label={"TOTAL LIQUIDITY\n\n"}
        content={O.fold(
          () => "",
          (t: TotalDeposits.Type): string =>
            printCurrencyUSD(TotalDeposits.iso.unwrap(t), {
              minimumFractionDigits: 2,
            })
        )(totalDeposits)}
      />
    )
  }

  function renderTotalLiquidityUtilization(
    totalLiquidityUtilization: O.Option<TotalLiquidityUtilization.Type>
  ) {
    return (
      <OverViewBox
        label={"LIQUIDITY UTILIZATION"}
        content={O.fold(
          () => "",
          (tlu: TotalLiquidityUtilization.Type): string =>
            `${TotalLiquidityUtilization.iso.unwrap(tlu)}`
        )(totalLiquidityUtilization)}
      />
    )
  }

  function renderTotalDailyVolumeUSD(
    totalDailyVolumeUSD: ByTxType<O.Option<TotalDailyVolume.Type>>
  ) {
    return (
      <OverViewBox
        label={"24H VOLUME\n\n"}
        content={O.fold(
          () => "",
          (tdv: TotalDailyVolume.Type): string =>
            printCurrencyUSD(TotalDailyVolume.iso.unwrap(tdv), {
              minimumFractionDigits: 2,
            })
        )(totalDailyVolumeUSD.trade)}
      />
    )
  }

  function renderTotalDailyFeeVolumeUSD(
    totalDailyFeeVolumeUSD: O.Option<TotalDailyFeeVolume.Type>
  ) {
    return (
      <OverViewBox
        label={"24H FEE GENERAGED"}
        content={O.fold(
          () => "",
          (tdfvs: TotalDailyFeeVolume.Type): string =>
            printCurrencyUSD(TotalDailyFeeVolume.iso.unwrap(tdfvs), {
              minimumFractionDigits: 2,
            })
        )(totalDailyFeeVolumeUSD)}
      />
    )
  }

  function renderTotalStats(rts: RemoteData<FetchDecodeError, TotalStats>) {
    switch (rts._tag) {
      case "Success":
        const ts: TotalStats = rts.success
        return (
          <>
            {" "}
            <Grid container item sm={12} md={6}>
              <Grid item xs={6} sm={3}>
                <OverViewBox label={"TVL\n\n"} content={"$220.21M"} />
              </Grid>
              <Grid item xs={6} sm={3}>
                {renderTotalDepositsAllPoolsUSD(ts.totalDepositsAllPoolsUSD)}
              </Grid>
              <Grid item xs={6} sm={3}>
                {renderTotalLiquidityUtilization(ts.totalLiquidityUtilization)}
              </Grid>
              <Grid item xs={6} sm={3}>
                {renderTotalDailyVolumeUSD(ts.totalDailyVolumeUSD)}
              </Grid>
            </Grid>
            <Grid container item sm={12} md={6}>
              <Grid item xs={6} sm={3}>
                {renderTotalDailyFeeVolumeUSD(ts.totalDailyFeeVolumeUSD)}
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
          </>
        )
      case "Pending":
        // TODO: loading
        return <span>loading …</span>
      case "Failure":
        // TODO: failure
        return (
          <details>
            <summary>Error</summary>
            <pre>{JSON.stringify(rts.failure, null, 2)}</pre>
          </details>
        )
    }
  }

  return <Box className={cx(classes.self)}>{renderTotalStats(totalStats)}</Box>
}

export default OverViewSection
