import React, { useMemo } from "react"
import {
  Box,
  CircularProgress,
  useMediaQuery,
  Typography,
  Button,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { RemoteData } from "fp-ts-remote-data"
// import * as ROM from "fp-ts/ReadonlyMap"

import { PoolsGrid } from "components"
import { FetchDecodeError } from "Data/FetchDecode"
import * as PoolSetName from "Data/Pool/PoolSetName"
import { PoolStats } from "Data/Stats/PoolStats"
import { usePoolStats } from "state/home/hooks"
import { useIsDarkMode } from "state/user/hooks"
import { useHistory } from "react-router-dom"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {},
  AllPools: {
    borderRadius: 100,
    background: `linear-gradient(90deg, ${palette.secondary.main} 0.3%, ${palette.secondary.dark} 100%)`,
    width: "300px",
    padding: 10,

    [`& > span`]: {
      color: palette.common.white,
      textTransform: "uppercase",
    },

    [breakpoints.down("xs")]: {
      width: "200px",
    },
  },
}))

const PoolsSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const history = useHistory()

  const poolStats: RemoteData<
    FetchDecodeError,
    ReadonlyMap<PoolSetName.Type, PoolStats>
  > = usePoolStats()

  const rows = useMemo(() => {
    if (poolStats._tag === "Success") {
      return [
        {
          id: 0,
          poolName: "PoolA",
          baseAPY: "2.99%",
          rewardsAPY: "+4.30% -> 10.76% DANA + 1.13% BTC",
          volume: "$2.55 M",
          APY: "29%",
        },
        {
          id: 1,
          poolName: "PoolB",
          baseAPY: "2.99%",
          rewardsAPY: "+4.30% -> 10.76% DANA + 1.13% BTC",
          volume: "$2.55 M",
          APY: "29%",
        },
        {
          id: 2,
          poolName: "PoolC",
          baseAPY: "2.99%",
          rewardsAPY: "+4.30% -> 10.76% DANA + 1.13% BTC",
          volume: "$2.55 M",
          APY: "29%",
        },
      ]
    }
    return []
  }, [poolStats])

  function renderPoolsGrid(
    psrd: RemoteData<FetchDecodeError, ReadonlyMap<PoolSetName.Type, PoolStats>>
  ): JSX.Element {
    switch (psrd._tag) {
      case "Success":
        return (
          <>
            <PoolsGrid rows={rows} />
            <Box display={"flex"} justifyContent={"flex-end"} mt={2}>
              <Button
                variant="contained"
                onClick={() => history.push("/pools")}
                className={classes.AllPools}
              >
                <Typography variant="h6" component="span">
                  See All Pools
                </Typography>
              </Button>
            </Box>
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

  return <Box className={classes.root}>{renderPoolsGrid(poolStats)}</Box>
}

export default PoolsSection
