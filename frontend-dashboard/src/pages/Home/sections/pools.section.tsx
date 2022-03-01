import React, { useMemo } from "react"
import { Box, CircularProgress, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import {
  GridCellParams,
  GridColDef,
  // GridValueGetterParams,
} from "@material-ui/data-grid"
import { RemoteData } from "fp-ts-remote-data"
import * as ROM from "fp-ts/ReadonlyMap"

import { PoolsGrid } from "components"
import { FetchDecodeError } from "Data/FetchDecode"
import * as PoolSetName from "Data/Pool/PoolSetName"
import { PoolStats } from "Data/Stats/PoolStats"
import { usePoolStats } from "state/home/hooks"
import { useIsDarkMode } from "state/user/hooks"

const useStyles = makeStyles(({ palette }) => ({
  root: {},
}))

const PoolsSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const poolStats: RemoteData<
    FetchDecodeError,
    ReadonlyMap<PoolSetName.Type, PoolStats>
  > = usePoolStats()

  const rows = useMemo(() => {
    if (poolStats._tag === "Success") {
      const rows = ROM.collect(PoolSetName.Ord)(
        (poolName: PoolSetName.Type, ps: PoolStats) => {
          return { ...ps, poolName }
        }
      )(poolStats.success)
      return rows
    }
    return []
  }, [poolStats])

  const columns: GridColDef[] = [
    {
      field: "poolName",
      headerName: "Pool",
      sortable: false,
      flex: 2,
      renderCell: (params: GridCellParams) => {
        let assetLogo = null
        // const type: string = params.getValue(params.id, "type") as string
        try {
          assetLogo = require(`assets/coins/${"BTC"}.png`).default
        } catch (e) {}

        return (
          <Box display="flex" alignItems="center">
            {assetLogo && (
              <img src={assetLogo} alt="" width="35px" height="35px" />
            )}
            <Box pl="15px">{params.value}</Box>
          </Box>
        )
      },
    },
    {
      field: "baseAPY",
      headerName: "Base APY",
      sortable: false,
      flex: 1,
      renderCell: (params: GridCellParams) => <Box>2.99%</Box>
    },
    {
      field: "rewardsAPY",
      headerName: "Rewards APY",
      sortable: false,
      flex: 2,
      renderCell: (params: GridCellParams) => <Box>{`+4.30% -> 10.76% DANA + 1.13% BTC`}</Box>
    },
    {
      field: "volume",
      headerName: "Volume",
      sortable: false,
      flex: 1,
      renderCell: (params: GridCellParams) => <Box>$2.55 M</Box>
    },
    {
      field: "APY",
      headerName: "APY",
      sortable: false,
      flex: 1,
      renderCell: (params: GridCellParams) => <Box>29%</Box>
    },
  ]

  function renderPoolsGrid(
    psrd: RemoteData<FetchDecodeError, ReadonlyMap<PoolSetName.Type, PoolStats>>
  ): JSX.Element {
    switch (psrd._tag) {
      case "Success":
        return (
          <PoolsGrid
            rows={[]}
            columns={columns}
            disableSelectionOnClick
            disableColumnSelector
            disableColumnMenu
            hideFooterPagination
            rowHeight={64}
            autoHeight
            pageSize={7}
          />
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
