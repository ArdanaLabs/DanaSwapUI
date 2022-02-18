import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import {
  GridCellParams,
  GridColDef,
  GridValueGetterParams,
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
    console.log(1111, poolStats.success)
    ROM.collect(PoolSetName.Ord)((a, b) => {
      console.log(222, a, b)
    })(poolStats.success)
  }

  const columns: GridColDef[] = [
    {
      field: "pool",
      headerName: "Pool",
      sortable: false,
      flex: 2,
      renderCell: (params: GridCellParams) => {
        let assetLogo = null
        const type: string = params.getValue(params.id, "type") as string
        try {
          // assetLogo = require(`assets/image/coins/${params.value}.png`).default
          assetLogo = require(`assets/image/coins/${"BTC"}.png`).default
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

  return (
    <Box className={classes.root}>
      {/* <PoolsGrid
        rows={filteredVaults}
        columns={columns}
        disableSelectionOnClick
        disableColumnSelector
        disableColumnMenu
        hideFooterPagination
        rowHeight={64}
        autoHeight
        pageSize={7}
      /> */}
    </Box>
  )
}

export default PoolsSection
