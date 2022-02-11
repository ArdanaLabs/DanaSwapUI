import React from "react"
import { Box, Grid, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import * as O from "fp-ts/Option"
import { RemoteData } from "fp-ts-remote-data"

import { ByTxType } from "Data/ByTxType"
import { FetchDecodeError } from "Data/FetchDecode"
import { TotalDailyVolume } from "Data/Stats/AggregateStats"
import { TotalStats } from "Data/Stats/CombinedStats"

import { printCurrencyUSD } from "hooks"
import { useIsDarkMode } from "state/user/hooks"
import { useTotalStats } from "state/home/hooks"

const useStyles = makeStyles(({ palette }) => ({
  self: {},
  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "28px",
    lineHeight: "110%",
    marginBottom: "20px",
    color: palette.secondary.main,
  },
  body: {
    "background": palette.background.paper,
    "padding": "30px 50px 30px 50px",
    "borderRadius": "10px",

    "& p": {
      "fontFamily": "Museo Sans",
      "fontStyle": "normal",
      "fontWeight": 900,
      "fontSize": "16px",
      "lineHeight": "20px",
      "whiteSpace": "pre-line",
      "color": palette.secondary.main,

      "& > span": {
        fontWeight: 100,
      },
    },
  },
  body_ex: {
    background: palette.background.paper,
    borderRadius: "25px",
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "28px",
    lineHeight: "43px",
    textAlign: "center",
    padding: "10px",
    color: palette.secondary.main,
  },
}))

const StatsSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const totalStats: RemoteData<FetchDecodeError, TotalStats> = useTotalStats()

  function renderTotalDailyVolumeUSD(
    totalDailyVolume: ByTxType<O.Option<TotalDailyVolume.Type>>
  ) {
    // TODO: text-transform: uppercase
    return (
      <Box className={cx(classes.body)}>
        <Box component="dl">
          <dt>DAILY DEPOSITS</dt>
          {O.fold(
            () => null,
            (tdv: TotalDailyVolume.Type) => {
              return (
                <dd>
                  {printCurrencyUSD(TotalDailyVolume.iso.unwrap(tdv))} (includes
                  factory pools)
                </dd>
              )
            }
          )(totalDailyVolume.addLiquidity)}
          <dt>DAILY WITHDRAWALS</dt>
          {O.fold(
            () => null,
            (tdv: TotalDailyVolume.Type) => {
              return (
                <dd>
                  {printCurrencyUSD(TotalDailyVolume.iso.unwrap(tdv))} (includes
                  factory pools)
                </dd>
              )
            }
          )(totalDailyVolume.removeLiquidity)}
          <dt>DAILY VOLUME</dt>
          {O.fold(
            () => null,
            (tdv: TotalDailyVolume.Type) => {
              return (
                <dd>{printCurrencyUSD(TotalDailyVolume.iso.unwrap(tdv))}</dd>
              )
            }
          )(totalDailyVolume.total)}
          <dt>STABLECOIN VOLUME</dt>
          {/* TODO: Intl.NumberFormat */}
          <dd>$3,065,174</dd>
        </Box>
      </Box>
    )
  }

  function renderTotalStats(rts: RemoteData<FetchDecodeError, TotalStats>) {
    switch (rts._tag) {
      case "Success":
        return renderTotalDailyVolumeUSD(rts.success.totalDailyVolumeUSD)
      case "Pending":
        // TODO: loading
        return <span>loading …</span>
      case "Failure":
        // TODO: failure
        return (
          <details>
            <summary> Error </summary>
            <pre>{JSON.stringify(rts.failure, null, 2)}</pre>
          </details>
        )
    }
  }

  return (
    <Box className={cx(classes.self)}>
      <Grid container spacing={3}>
        <Grid item xs={12} sm={6}>
          <Box className={cx(classes.title)}>exDANA Stats</Box>
          <Box className={cx(classes.body)}>
            <Box component="p">
              {/* TODO: remove <br>, text-transform: uppercase */}
              exDANA holder/LP ratio (based on fees): <span>24.52</span>
              <br />
              <br />
              Having locked $1 in DANA for 4 years is equal to having provided
              $24.52 as an LP
              <br />
              <br />
              exDANA holder APY: <span>21.37% (4 weeks average: 18.01%)</span>
              <br />
              Yearly fee earnings per 1 exDANA: <span>$0.34</span>
              <br />
              <br />
              My exDANA balance: <span>0 Stake DANA</span>
              <br />
              <br />
              Averaged daily earnings: <span>$198,244.20</span>
              <br />
              <br />
              Weekly earnings: <span>$1,387,709.41</span>
              <br />
              <br />
              Weekly volume (including deposits/withdrawals):{" "}
              <span>$6,938,547,054.70</span>
              <br />
              <br />
              Next Distribution: <span>Mon, 28 Jun 2021 23:20:50 GMT</span>
            </Box>
          </Box>
        </Grid>
        <Grid item xs={12} sm={6}>
          <Box className={cx(classes.title)}>
            Total Pool Deposits and Daily Volume
          </Box>

          {renderTotalStats(totalStats)}

          <Box mt="30px" />

          <Box className={cx(classes.title)}>exDANA 2pool LP Claim:</Box>
          <Box className={cx(classes.body_ex)}>Claim 9999.122</Box>
        </Grid>
      </Grid>
    </Box>
  )
}

export default StatsSection
