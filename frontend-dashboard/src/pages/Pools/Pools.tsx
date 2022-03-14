import React, { ChangeEvent, useState } from "react"
import {
  Box,
  Button,
  Fade,
  useMediaQuery,
  Container,
  Typography,
  CircularProgress,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import * as O from "fp-ts/Option"
import { RemoteData } from "fp-ts-remote-data"

import { FetchDecodeError } from "Data/FetchDecode"
import { TotalDeposits, TotalDailyVolume } from "Data/Stats/AggregateStats"
import { TotalStats } from "Data/Stats/CombinedStats"
import * as Theme from "Data/User/Theme"

import { GradientBox, PoolsGrid } from "components"
import { printCurrencyUSD } from "hooks"
import { useUserTheme } from "state/user/hooks"
import cx from "classnames"
import { usePoolStats, useTotalStats } from "state/home/hooks"

import CyanBG from "assets/backgrounds/cyan.svg"
import PinkBG from "assets/backgrounds/pink.svg"
import { PoolStats } from "Data/Stats/PoolStats"
import * as PoolSetName from "Data/Pool/PoolSetName"

import { ReactComponent as SearchIcon } from "assets/imgs/search.svg"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: `url(${PinkBG}) right -600px top -600px no-repeat,
                  url(${CyanBG}) left -800px top -200px no-repeat`,
    paddingTop: "180px",
    paddingBottom: "50px",

    display: "flex",
    justifyContent: "center",
    alignItems: "center",
  },
  title: {
    color: palette.primary.main,
    margin: "30px 0",
    fontWeight: 900,
  },
  filterBar: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",

    [`& h6`]: {
      color: palette.common.white,
      textTransform: "uppercase",
    },

    [breakpoints.down("xs")]: {
      flexDirection: "column",
      gap: "10px",
    },
  },
  filterByInput: {
    position: "relative",

    [breakpoints.down("xs")]: {
      width: "100%",
    },

    [`& > input`]: {
      background: "transparent",
      outline: "unset",
      border: `1px solid ${palette.secondary.main}88`,
      borderRadius: 100,
      padding: "15px 50px",
      width: 500,
      color: palette.primary.main,
      fontFamily: "Museo Sans",
      fontWeight: 600,
      fontSize: "13px",
      lineHeight: "100%",

      [`&::placeholder`]: {
        color: palette.primary.main,
      },

      [breakpoints.down("xs")]: {
        width: "100%",
      },
    },
    [`& > .icon`]: {
      position: "absolute",
      top: "50%",
      left: "30px",
      transform: "translate(-50%, -50%)",

      [`& path`]: {
        fill: palette.primary.main,
      },
    },
  },
  filterByTypeButton: {
    display: "flex",
    gap: "0 20px",

    [breakpoints.down("xs")]: {
      justifyContent: "space-between",
      width: "100%",
    },

    [`& > .MuiButton-root`]: {
      background: `linear-gradient(90deg, ${palette.secondary.main} 0%, ${palette.secondary.dark} 100%)`,
      borderRadius: "50px",
      width: 150,
    },
  },

  statsPanel: {
    background: `linear-gradient(126.33deg, ${palette.background.paper} 9.83%, #00000000 96.44%);`,
    padding: 30,
    borderRadius: "10px",

    [`& dt, & dd`]: {
      display: "inline-block",
      color: palette.primary.main,

      [breakpoints.down("xs")]: {
        fontSize: 12,
      },
    },

    [`& dd`]: {
      fontWeight: 300,
    },
  },
}))

enum PoolFilterType {
  STABLECOINS = "Stablecoins",
  DIGITAL_ASSETS = "Digital Assets",
}

const Pools: React.FC = () => {
  const { breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const userTheme: Theme.Theme = useUserTheme()
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })
  const [filterBy, setFilterBy] = useState({
    input: "",
    type: PoolFilterType.STABLECOINS,
  })

  const totalStats: RemoteData<FetchDecodeError, TotalStats> = useTotalStats()
  const poolStats: RemoteData<
    FetchDecodeError,
    ReadonlyMap<PoolSetName.Type, PoolStats>
  > = usePoolStats()

  function renderFilterBar(): JSX.Element {
    return (
      <Box className={classes.filterBar} mb={!mobile ? 10 : 5}>
        <Box className={classes.filterByInput}>
          <input
            value={filterBy.input}
            placeholder="Search pool by name, network or type..."
            onChange={(e: ChangeEvent<HTMLInputElement>) => {
              const input = e.target.value
              setFilterBy((prev) => ({
                ...prev,
                input,
              }))
            }}
          />
          <SearchIcon className={"icon"} />
        </Box>
        <Box className={classes.filterByTypeButton}>
          {[PoolFilterType.STABLECOINS, PoolFilterType.DIGITAL_ASSETS].map(
            (filterType) =>
              filterType === filterBy.type ? (
                <Button
                  key={filterType}
                  variant="contained"
                  onClick={() => {
                    setFilterBy((prev) => ({
                      ...prev,
                      type: filterType,
                    }))
                  }}
                >
                  <Typography variant="h6" component="h6">
                    {filterType}
                  </Typography>
                </Button>
              ) : (
                <GradientBox
                  key={filterType}
                  width={150}
                  height={43}
                  glow={false}
                  onClick={() => {
                    setFilterBy((prev) => ({
                      ...prev,
                      type: filterType,
                    }))
                  }}
                >
                  <Typography variant="h6" component="h6">
                    {filterType}
                  </Typography>
                </GradientBox>
              )
          )}
        </Box>
      </Box>
    )
  }

  function renderPools(
    psrd: RemoteData<FetchDecodeError, ReadonlyMap<PoolSetName.Type, PoolStats>>
  ): JSX.Element {
    switch (psrd._tag) {
      case "Success":
        return (
          <>
            {renderFilterBar()}
            <PoolsGrid rows={psrd.success} />
          </>
        )
      case "Pending":
        return (
          <Box display="flex" justifyContent="center" alignItems="center">
            <CircularProgress />
          </Box>
        )
      case "Failure":
        return (
          <details>
            <summary>Error</summary>
            <pre>{JSON.stringify(psrd.failure, null, 2)}</pre>
          </details>
        )
    }
  }

  const renderPoolStats = () => {
    switch (totalStats._tag) {
      case "Success":
        const ts: TotalStats = totalStats.success
        return (
          <Box className={cx(classes.statsPanel)}>
            <Box component="dl">
              <Box mb={!mobile ? "20px" : "10px"}>
                <Typography variant="h4" component="dt">
                  Deposit:&nbsp;
                </Typography>
                <Typography variant="h4" component="dd">
                  {O.fold(
                    () => "0",
                    (td: TotalDeposits.Type) =>
                      printCurrencyUSD(TotalDeposits.iso.unwrap(td))
                  )(ts.totalDepositsAllPoolsUSD)}{" "}
                  (includes factory pools)
                </Typography>
              </Box>
              <Box mb={!mobile ? "20px" : "10px"}>
                <Typography variant="h4" component="dt">
                  Daily Volume:&nbsp;
                </Typography>
                <Typography variant="h4" component="dd">
                  {O.fold(
                    () => null,
                    (tdv: TotalDailyVolume.Type) =>
                      printCurrencyUSD(TotalDailyVolume.iso.unwrap(tdv))
                  )(ts.totalDailyVolumeUSD.trade)}
                </Typography>
              </Box>
              <Box mb={!mobile ? "20px" : "10px"}>
                <Typography variant="h4" component="dt">
                  Factory Daily Volume:&nbsp;
                </Typography>
                <Typography variant="h4" component="dd">
                  $8,999,777
                </Typography>
              </Box>
            </Box>
          </Box>
        )
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
            <summary>Error</summary>
            <pre>{JSON.stringify(totalStats.failure, null, 2)}</pre>
          </details>
        )
    }
  }

  return (
    <Fade in={true}>
      <Box className={cx(classes.root)}>
        <Container>
          <Typography variant="h1" component="h1" className={classes.title}>
            DANA Pools
          </Typography>

          {renderPools(poolStats)}

          <Box mb={10} />

          <Typography variant="h1" component="h1" className={classes.title}>
            Total pool deposits and daily volume
          </Typography>

          {renderPoolStats()}
        </Container>
      </Box>
    </Fade>
  )
}

export default Pools
