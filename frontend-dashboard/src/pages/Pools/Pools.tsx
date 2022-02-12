import React from "react"
import { Box, Fade, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import * as O from "fp-ts/Option"
import { RemoteData } from "fp-ts-remote-data"

import { FetchDecodeError } from "Data/FetchDecode"
import { TotalDeposits, TotalDailyVolume } from "Data/Stats/AggregateStats"
import { TotalStats } from "Data/Stats/CombinedStats"

import { PoolsPanel } from "components"
import { printCurrencyUSD } from "hooks"
import { useIsDarkMode } from "state/user/hooks"
import cx from "classnames"
import { useTotalStats } from "state/home/hooks"

const useStyles = makeStyles(({ palette }) => ({
  label: {
    color: palette.secondary.main,
    fontFamily: "'Brandon Grotesque'",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "28px",
    lineHeight: "110%",
    margin: "30px 0",
  },
  statsPanel: {
    "background":
      palette.type === "light" ? "#F6F6F6" : palette.background.paper,
    "borderRadius": "10px",
    "border": palette.type === "light" ? "1px solid #C4C4C4" : "unset",
    "padding": "50px 70px",

    "fontFamily": "Museo Sans",
    "fontStyle": "normal",
    "fontWeight": 900,
    "fontSize": "16px",
    "lineHeight": "115%",
    "whiteSpace": "pre-line",

    "color": palette.text.secondary,

    "& span": {
      fontWeight: 300,
    },
  },
}))

const Pools: React.FC = () => {
  const { breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const dark = useIsDarkMode()
  const classes = useStyles({ dark, mobile })

  const totalStats: RemoteData<FetchDecodeError, TotalStats> = useTotalStats()

  switch (totalStats._tag) {
    case "Success":
      const ts: TotalStats = totalStats.success
      return (
        <Fade in={true}>
          <Box>
            <Box className={cx(classes.label)}>Dana Pools</Box>
            <PoolsPanel />

            <Box className={cx(classes.label)}>
              Total pool deposits and daily volume
            </Box>
            <Box className={cx(classes.statsPanel)}>
              {/* TODO: use <dt> or <table> for key-value pairs */}
              Deposit:{" "}
              <span>
                {O.fold(
                  () => "",
                  (td: TotalDeposits.Type) =>
                    printCurrencyUSD(TotalDeposits.iso.unwrap(td))
                )(ts.totalDepositsAllPoolsUSD)}{" "}
                (includes factory pools)
              </span>
              <br />
              <br />
              Daily Volume:{" "}
              {O.fold(
                () => null,
                (tdv: TotalDailyVolume.Type) => {
                  return (
                    <span>
                      {printCurrencyUSD(TotalDailyVolume.iso.unwrap(tdv))}
                    </span>
                  )
                }
              )(ts.totalDailyVolumeUSD.trade)}
              <br />
              <br />
              {/* TODO: Intl.NumberFormat */}
              Factory Daily Volume: <span>$8,999,777</span>
            </Box>
          </Box>
        </Fade>
      )
    case "Pending":
      // TODO: loading
      return <span>loading …</span>
    case "Failure":
      // TODO: failure
      return (
        <details>
          <summary>Error</summary>
          <pre>{JSON.stringify(totalStats.failure, null, 2)}</pre>
        </details>
      )
  }
}

export default Pools
