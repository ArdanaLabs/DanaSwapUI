import React from "react"
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

  if (poolStats._tag === "Success") {
    ROM.collect(PoolSetName.Ord)(
      (poolName: PoolSetName.Type, ps: PoolStats) => {}
    )(poolStats.success)
  }

  const columns: GridColDef[] = [
    {
      field: "pool",
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
      field: "type",
      headerName: "Type",
      sortable: false,
      flex: 1,
      renderCell: (params: GridCellParams) => <Box>{params.value}</Box>,
    },
    {
      field: "locked",
      headerName: "dUSD Available",
      flex: 1,
    },
    {
      field: "stabilityFee",
      headerName: "Stability Fee",
      type: "number",
      flex: 1,
      align: "left",
      headerAlign: "left",
    },
    {
      field: "minCollRatio",
      headerName: "Min Coll. Ratio",
      type: "number",
      flex: 1,
      align: "left",
      headerAlign: "left",
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
