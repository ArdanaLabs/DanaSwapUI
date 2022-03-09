import React from "react"
import cx from "classnames"

import { Typography, Box, useMediaQuery } from "@material-ui/core"
import { withStyles } from "@material-ui/core/styles"
import {
  DataGrid,
  GridCellParams,
  GridColDef,
  GridColumnHeaderParams,
} from "@material-ui/data-grid"
import { useIsDarkMode } from "state/user/hooks"
import { useTheme, makeStyles } from "@material-ui/core/styles"

const StyledDataGrid = withStyles((theme) => ({
  root: {
    border: "unset",
    [`& .MuiDataGrid-columnHeaderTitleContainer`]: {
      padding: "0 10px !important",
    },
    [`& .MuiDataGrid-columnHeaderTitle`]: {
      [`& svg`]: {
        color: `${theme.palette.primary.main} !important`,
      },
    },
    [`& .MuiDataGrid-columnSeparator`]: {
      opacity: "0 !important",
      border: "unset",
    },
    [`& .MuiDataGrid-cell:focus, & .MuiDataGrid-columnHeader:focus`]: {
      outline: "none !important",
    },
    [`& .MuiDataGrid-columnHeader:focus-within, & .MuiDataGrid-cell:focus-within`]:
      {
        outline: "none !important",
      },
    [`& .MuiDataGrid-window, & .MuiDataGrid-viewport`]: {
      overflow: "unset !important",
    },
    [`& .MuiDataGrid-cell`]: {
      border: "unset !important",
    },
    [`& .MuiDataGrid-columnsContainer`]: {
      border: "unset !important",
    },
  },
  columnHeader: {},
  row: {
    border: `1px solid ${theme.palette.secondary.main}88`,
    borderRadius: 50,
    // width: "100% !important",
    padding: "0 10px",
    cursor: "pointer",
    margin: "15px auto",
  },
  cell: {
    border: "unset !important",
  },
}))(DataGrid)

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {},
  assetLogoImg: {
    width: 40,
    height: 40,

    [breakpoints.down("xs")]: {
      width: 20,
      height: 20,
    },
  },
  gridHeaderColumnTextStyle: {
    color: palette.secondary.main,
    textTransform: "uppercase",
    fontWeight: 900,
  },
  gridCellTextStyle: {
    color: palette.primary.main,
    fontWeight: 300,

    [`&.bolder`]: {
      fontWeight: 900,
    },

    [`&.smaller`]: {
      fontSize: 16,
      [breakpoints.down("xs")]: {
        fontSize: 10,
      },
    },
  },
}))

interface PoolGridDataField {
  id: number
  poolName: string
  baseAPY: string
  rewardsAPY: string
  volume: string
  APY: string
}
interface Props {
  rows: PoolGridDataField[]
}

const PoolsGrid: React.FC<Props> = ({ rows }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const columns: GridColDef[] = [
    {
      field: "poolName",
      headerName: "Pool",
      sortable: false,
      flex: 2,
      minWidth: 250,
      renderHeader: (params: GridColumnHeaderParams) => (
        <Typography
          variant="h5"
          component="span"
          className={cx(classes.gridHeaderColumnTextStyle)}
        >
          {params.colDef.headerName}
        </Typography>
      ),
      renderCell: (params: GridCellParams) => {
        let assetLogo = null
        // const type: string = params.getValue(params.id, "type") as string
        try {
          assetLogo = require(`assets/coins/${"BTC"}.png`).default
        } catch (e) {}

        return (
          <Box display="flex" alignItems="center">
            {assetLogo && (
              <Box display={"flex"} mr={2} className={classes.assetLogoImg}>
                <img src={assetLogo} alt="AssetLogo" />
              </Box>
            )}
            <Box display={"flex"} flexDirection={"column"}>
              <Typography
                variant="h4"
                component="span"
                className={cx(classes.gridCellTextStyle, "bolder", "smaller")}
              >
                {params.value}
              </Typography>
              <Typography
                variant="h4"
                component="span"
                className={cx(classes.gridCellTextStyle, "smaller")}
              >
                DAI + USDC + USDT + sUSD
              </Typography>
            </Box>
          </Box>
        )
      },
    },
    {
      field: "baseAPY",
      headerName: "Base APY",
      sortable: false,
      flex: 1,
      minWidth: 100,
      renderHeader: (params: GridColumnHeaderParams) => (
        <Typography
          variant="h5"
          component="span"
          className={cx(classes.gridHeaderColumnTextStyle)}
        >
          {params.colDef.headerName}
        </Typography>
      ),
      renderCell: (params: GridCellParams) => (
        <Typography
          variant="h4"
          component="span"
          className={cx(classes.gridCellTextStyle, "bolder")}
        >
          {params.value}
        </Typography>
      ),
    },
    {
      field: "rewardsAPY",
      headerName: "Rewards APY",
      sortable: false,
      flex: 2,
      minWidth: 250,
      renderHeader: (params: GridColumnHeaderParams) => (
        <Typography
          variant="h5"
          component="span"
          className={cx(classes.gridHeaderColumnTextStyle)}
        >
          {params.colDef.headerName}
        </Typography>
      ),
      renderCell: (params: GridCellParams) => (
        <Typography
          variant="h4"
          component="span"
          className={cx(classes.gridCellTextStyle)}
        >
          {params.value}
        </Typography>
      ),
    },
    {
      field: "volume",
      headerName: "Volume",
      sortable: false,
      flex: 1,
      minWidth: 100,
      renderHeader: (params: GridColumnHeaderParams) => (
        <Typography
          variant="h5"
          component="span"
          className={cx(classes.gridHeaderColumnTextStyle)}
        >
          {params.colDef.headerName}
        </Typography>
      ),
      renderCell: (params: GridCellParams) => (
        <Typography
          variant="h4"
          component="span"
          className={cx(classes.gridCellTextStyle, "bolder")}
        >
          {params.value}
        </Typography>
      ),
    },
    {
      field: "APY",
      headerName: "APY",
      sortable: false,
      flex: 1,
      minWidth: 50,
      renderHeader: (params: GridColumnHeaderParams) => (
        <Typography
          variant="h5"
          component="span"
          className={cx(classes.gridHeaderColumnTextStyle)}
        >
          {params.colDef.headerName}
        </Typography>
      ),
      renderCell: (params: GridCellParams) => (
        <Typography
          variant="h4"
          component="span"
          className={cx(classes.gridCellTextStyle, "bolder")}
        >
          {params.value}
        </Typography>
      ),
    },
  ]

  return (
    <Box className={classes.root}>
      <StyledDataGrid
        rows={rows}
        columns={columns}
        disableSelectionOnClick
        disableColumnSelector
        disableColumnMenu
        hideFooterPagination
        rowHeight={!mobile ? 76 : 35}
        headerHeight={30}
        autoHeight
        pageSize={7}
      />
    </Box>
  )
}

export default PoolsGrid
