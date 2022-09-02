import React from "react"
import {
  Box,
  Grid,
  useMediaQuery,
  Typography,
  CircularProgress,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import * as O from "fp-ts/Option"
import { RemoteData } from "fp-ts-remote-data"

import { ByTxType } from "Data/ByTxType"
import { FetchDecodeError } from "Data/FetchDecode"
import { TotalDailyVolume } from "Data/Stats/AggregateStats"
import { TotalStats } from "Data/Stats/CombinedStats"
import * as Theme from "Data/User/Theme"

import { printCurrencyUSD } from "hooks"
import { useUserTheme } from "state/user/hooks"
import { useTotalStats } from "state/home/hooks"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {},
  title: {
    color: palette.primary.main,
    marginBottom: 50,
    [breakpoints.down("sm")]: {
      marginBottom: 30,
    },
  },
  body: {
    background: `linear-gradient(126.33deg, ${palette.background.paper} 9.83%, #00000000 96.44%);`,
    padding: 30,
    borderRadius: "10px",

    [`& .totalDailyVolume`]: {
      margin: 0,
      [`& dt`]: {
        textTransform: "uppercase",
      },
    },

    [`& .exDANAStats`]: {
      margin: 0,
      [`& dt, & dd`]: {
        display: "inline-block",
      },
    },

    [`& dd`]: {
      fontWeight: 300,
    },
    [`& dt, & dd`]: {
      color: palette.primary.main,
      [breakpoints.down("xs")]: {
        fontSize: 12,
      },
    },

    [breakpoints.down("xs")]: {
      padding: 20,
      marginBottom: 30,
    },
  },
}))

const StatsSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

  const totalStats: RemoteData<FetchDecodeError, TotalStats> = useTotalStats()

  function renderTotalDailyVolumeUSD(
    totalDailyVolume: ByTxType<O.Option<TotalDailyVolume.Type>>
  ) {
    return (
      <Box component="dl" className="totalDailyVolume">
        <Typography variant="h4" component="dt">
          Daily Deposits:
        </Typography>
        {O.fold(
          () => null,
          (tdv: TotalDailyVolume.Type) => (
            <Typography variant="h4" component="dd">
              {printCurrencyUSD(TotalDailyVolume.iso.unwrap(tdv))} (includes
              factory pools)
            </Typography>
          )
        )(totalDailyVolume.addLiquidity)}

        <Box mb={!mobile ? "20px" : "10px"} />

        <Typography variant="h4" component="dt">
          Daily Withdrawals:
        </Typography>
        {O.fold(
          () => null,
          (tdv: TotalDailyVolume.Type) => (
            <Typography variant="h4" component="dd">
              {printCurrencyUSD(TotalDailyVolume.iso.unwrap(tdv))} (includes
              factory pools)
            </Typography>
          )
        )(totalDailyVolume.removeLiquidity)}

        <Box mb={!mobile ? "20px" : "10px"} />

        <Typography variant="h4" component="dt">
          Daily Volume:
        </Typography>
        {O.fold(
          () => null,
          (tdv: TotalDailyVolume.Type) => (
            <Typography variant="h4" component="dd">
              {printCurrencyUSD(TotalDailyVolume.iso.unwrap(tdv))}
            </Typography>
          )
        )(totalDailyVolume.total)}
      </Box>
    )
  }

  function renderExDANAStatsSuccess() {
    return (
      <Box component="dl" className="exDANAStats">
        <Box mb={!mobile ? "20px" : "10px"}>
          <Typography variant="h4" component="dt">
            exDANA holder/LP ratio (based on fees):&nbsp;
          </Typography>
          <Typography variant="h4" component="dd">
            24.52
          </Typography>
        </Box>
        <Box mb={!mobile ? "20px" : "10px"}>
          <Typography variant="h4" component="dt">
            Having locked $1 in DANA for 4 years is equal to having provided
            $24.52 as an LP
          </Typography>
        </Box>
        <Box>
          <Typography variant="h4" component="dt">
            exDANA holder APY:&nbsp;
          </Typography>
          <Typography variant="h4" component="dd">
            21.37% (4 weeks average: 18.01%)
          </Typography>
        </Box>
        <Box mb={!mobile ? "20px" : "10px"}>
          <Typography variant="h4" component="dt">
            Yearly fee earnings per 1 exDANA:&nbsp;
          </Typography>
          <Typography variant="h4" component="dd">
            $0.34
          </Typography>
        </Box>
        <Box mb={!mobile ? "20px" : "10px"}>
          <Typography variant="h4" component="dt">
            My exDANA balance:&nbsp;
          </Typography>
          <Typography variant="h4" component="dd">
            0 Stake DANA
          </Typography>
        </Box>
        <Box mb={!mobile ? "20px" : "10px"}>
          <Typography variant="h4" component="dt">
            Averaged daily earnings:&nbsp;
          </Typography>
          <Typography variant="h4" component="dd">
            $198,244.20
          </Typography>
        </Box>
        <Box mb={!mobile ? "20px" : "10px"}>
          <Typography variant="h4" component="dt">
            Weekly earnings:&nbsp;
          </Typography>
          <Typography variant="h4" component="dd">
            $1,387,709.41
          </Typography>
        </Box>
        <Box mb={!mobile ? "20px" : "10px"}>
          <Typography variant="h4" component="dt">
            Weekly volume (including deposits/withdrawals):&nbsp;
          </Typography>
          <Typography variant="h4" component="dd">
            $6,938,547,054.70
          </Typography>
        </Box>
        <Box>
          <Typography variant="h4" component="dt">
            Next Distribution:&nbsp;
          </Typography>
          <Typography variant="h4" component="dd">
            Mon, 28 Jun 2021 23:20:50 GMT
          </Typography>
        </Box>
      </Box>
    )
  }

  function renderLPClaimSuccess() {
    return (
      <Box textAlign={"center"}>
        <Typography variant="h3" component="dt">
          Claim 9999.122
        </Typography>
      </Box>
    )
  }

  function renderTotalStats(rts: RemoteData<FetchDecodeError, TotalStats>) {
    switch (rts._tag) {
      case "Success":
        return renderTotalDailyVolumeUSD(rts.success.totalDailyVolumeUSD)
      case "Pending":
        return (
          <Box display="flex" justifyContent="center" alignItems="center">
            <CircularProgress />
          </Box>
        )
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

  function renderExDANAStats(rts: RemoteData<FetchDecodeError, TotalStats>) {
    switch (rts._tag) {
      case "Success":
        return renderExDANAStatsSuccess()
      case "Pending":
        return (
          <Box display="flex" justifyContent="center" alignItems="center">
            <CircularProgress />
          </Box>
        )
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

  function renderLPClaim(rts: RemoteData<FetchDecodeError, TotalStats>) {
    switch (rts._tag) {
      case "Success":
        return renderLPClaimSuccess()
      case "Pending":
        return (
          <Box display="flex" justifyContent="center" alignItems="center">
            <CircularProgress />
          </Box>
        )
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
    <Box className={classes.root}>
      <Grid container spacing={3}>
        <Grid item xs={12} sm={6}>
          <Typography variant="h1" component="h1" className={classes.title}>
            exDANA Stats
          </Typography>
          <Box className={classes.body}>{renderExDANAStats(totalStats)}</Box>
        </Grid>
        <Grid item xs={12} sm={6}>
          <Typography variant="h1" component="h1" className={classes.title}>
            Total Pool Deposits and Daily Volume
          </Typography>

          <Box className={classes.body}>{renderTotalStats(totalStats)}</Box>

          <Box mt="50px" />

          <Typography variant="h1" component="h1" className={classes.title}>
            exDANA 2pool LP Claim:
          </Typography>
          <Box className={classes.body} padding={"12px !important"}>
            {renderLPClaim(totalStats)}
          </Box>
        </Grid>
      </Grid>
    </Box>
  )
}

export default StatsSection
