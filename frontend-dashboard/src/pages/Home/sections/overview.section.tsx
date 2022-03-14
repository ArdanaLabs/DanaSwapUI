import React from "react"
import { Box, Grid, useMediaQuery } from "@mui/material"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

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
import * as Theme from "Data/User/Theme"

import { useTotalStats } from "state/home/hooks"
import { printCurrencyUSD } from "hooks"
import { useUserTheme } from "state/user/hooks"
import { OverViewBox } from "components/Box"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    // background: "transparent",
    // margin: "auto -10px",
  },
}))

const OverViewSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

  const totalStats: RemoteData<FetchDecodeError, TotalStats> = useTotalStats()

  function renderTotalDepositsAllPoolsUSD(
    totalDeposits: O.Option<TotalDeposits.Type>
  ) {
    return (
      <OverViewBox
        label={"Total Liquidity\n\n"}
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
        label={"Total Liquidity\nUtilization"}
        content={O.fold(
          () => "",
          (tlu: TotalLiquidityUtilization.Type): string =>
            printCurrencyUSD(TotalLiquidityUtilization.iso.unwrap(tlu), {
              minimumFractionDigits: 2,
            })
        )(totalLiquidityUtilization)}
      />
    )
  }

  function renderTotalDailyVolumeUSD(
    totalDailyVolumeUSD: ByTxType<O.Option<TotalDailyVolume.Type>>
  ) {
    return (
      <OverViewBox
        label={"24hr Volume\n\n"}
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
        label={"24h fee Generated"}
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
          <Grid container columns={8} spacing={1}>
            <Grid item xs={4} sm={2} md={1}>
              <OverViewBox label={"TVL\n\n"} content={"$220.21M"} />
            </Grid>
            <Grid item xs={4} sm={2} md={1}>
              {renderTotalDepositsAllPoolsUSD(ts.totalDepositsAllPoolsUSD)}
            </Grid>
            <Grid item xs={4} sm={2} md={1}>
              {renderTotalLiquidityUtilization(ts.totalLiquidityUtilization)}
            </Grid>
            <Grid item xs={4} sm={2} md={1}>
              {renderTotalDailyVolumeUSD(ts.totalDailyVolumeUSD)}
            </Grid>
            <Grid item xs={4} sm={2} md={1}>
              {renderTotalDailyFeeVolumeUSD(ts.totalDailyFeeVolumeUSD)}
            </Grid>
            <Grid item xs={4} sm={2} md={1}>
              <OverViewBox label={"Dana Price\n\n"} content={"$220.21M"} />
            </Grid>
            <Grid item xs={4} sm={2} md={1}>
              <OverViewBox label={"% of Dana Locked"} content={"30.11%"} />
            </Grid>
            <Grid item xs={4} sm={2} md={1}>
              <OverViewBox label={"Dana Staking\nAPY"} content={"30.11%"} />
            </Grid>
          </Grid>
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

  return <Box className={cx(classes.root)}>{renderTotalStats(totalStats)}</Box>
}

export default OverViewSection
