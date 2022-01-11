import React, { useState } from "react"
import { Box, Container, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"
import {
  TokenAssetGrid,
  TokenAssetGridFilter,
  TokenAssetCard,
  VaultButton,
} from "components"
import {
  GridCellParams,
  GridColDef,
  GridValueGetterParams,
} from "@material-ui/data-grid"
import {
  FilterOption,
  FilterType,
} from "components/DataGrid/TokenAssetGridFilter"

import { useUiModal } from "state/ui/hooks"
import { useVault } from "state/vault/hooks"
import { numberFormatter, percentageFormatter } from "hooks/formatter"

// const rows = [
//   {
//     id: 0,
//     asset: "Wrapped Bitcoin1",
//     type: "WBTC-W",
//     dUSD: "29.36M",
//     stabilityFee: 2,
//     minColl: 150,
//     assetIcon: COIN1,
//   },
//   {
//     id: 17,
//     asset: "Wrapped Bitcoin2",
//     type: "WBTC-Q",
//     dUSD: "29.36M",
//     stabilityFee: 2,
//     minColl: 150,
//     assetIcon: COIN1,
//   },
//   {
//     id: 18,
//     asset: "Wrapped Bitcoin3",
//     type: "WBTC-E",
//     dUSD: "29.36M",
//     stabilityFee: 1,
//     minColl: 150,
//     assetIcon: COIN1,
//   },
//   {
//     id: 19,
//     asset: "Wrapped Bitcoin4",
//     type: "WBTC-D",
//     dUSD: "29.36M",
//     stabilityFee: 3,
//     minColl: 150,
//     assetIcon: COIN1,
//   },
//   {
//     id: 110,
//     asset: "Wrapped Bitcoin5",
//     type: "WBTC-C",
//     dUSD: "29.36M",
//     stabilityFee: 1,
//     minColl: 150,
//     assetIcon: COIN1,
//   },
//   {
//     id: 120,
//     asset: "Wrapped Bitcoin6",
//     type: "WBTC-A",
//     dUSD: "29.36M",
//     stabilityFee: 20,
//     minColl: 150,
//     assetIcon: COIN1,
//   },
//   {
//     id: 130,
//     asset: "Wrapped Bitcoin7",
//     type: "WBTC-B",
//     dUSD: "29.36M",
//     stabilityFee: 2,
//     minColl: 150,
//     assetIcon: COIN1,
//   },
// ]

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {},
  filter: {},
  assetList: {
    width: "100%",
  },
  sortIcon: {
    color: palette.primary.main,
    fontSize: "10px",
  },
  uppercase: {
    textTransform: "uppercase",
  },
}))

const AssetSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const [filterOption, setFilterOption] = useState<FilterOption>({
    filterType: FilterType.POPULAR,
    keyword: "",
  })

  const { toggleModal } = useUiModal()
  const { vaults } = useVault()

  const columns: GridColDef[] = [
    {
      field: "asset",
      headerName: "Asset",
      sortable: false,
      flex: 2,
      renderCell: (params: GridCellParams) => {
        const assetLogo =
          require(`assets/image/coins/${params.value}.svg`).default
        return (
          <Box display="flex" alignItems="center">
            {assetLogo && (
              <img src={assetLogo} alt="" width="35px" height="35px" />
            )}
            <Box pl="15px" className={classes.uppercase}>
              {params.value}
            </Box>
          </Box>
        )
      },
    },
    {
      field: "type",
      headerName: "Type",
      sortable: false,
      flex: 1,
      renderCell: (params: GridCellParams) => (
        <Box className={classes.uppercase}>{params.value}</Box>
      ),
    },
    {
      field: "locked",
      headerName: "dUSD Available",
      flex: 1,
      valueGetter: (params: GridValueGetterParams) =>
        numberFormatter((params.value as number) ?? 0),
    },
    {
      field: "stabilityFee",
      headerName: "Stability Fee",
      type: "number",
      flex: 1,
      align: "left",
      headerAlign: "left",
      valueGetter: (params: GridValueGetterParams) =>
        percentageFormatter(params.value as number),
    },
    {
      field: "minCollRatio",
      headerName: "Min Coll. Ratio",
      type: "number",
      flex: 1,
      align: "left",
      headerAlign: "left",
      valueGetter: (params: GridValueGetterParams) =>
        percentageFormatter(params.value as number, 0),
    },
    {
      field: "action",
      headerName: " ",
      sortable: false,
      width: 200,
      align: "center",
      renderCell: (params: GridCellParams) => {
        const type: string = params.getValue(params.id, "type") as string
        return (
          <VaultButton onClick={() => handleOpenVault(type)}>
            Open Vault
          </VaultButton>
        )
      },
    },
  ]

  const handleOpenVault = (type: string) => {
    if (!type) {
      return
    }

    toggleModal({
      open: true,
      type,
    })
  }

  const SortedDescendingIcon = () => (
    <Box display="flex" justifyContent="center" alignItems="center">
      <i
        className={cx("fa fa-chevron-down", classes.sortIcon)}
        aria-hidden="true"
      />
    </Box>
  )

  const SortedAscendingIcon = () => (
    <Box display="flex" justifyContent="center" alignItems="center">
      <i
        className={cx("fa fa-chevron-up", classes.sortIcon)}
        aria-hidden="true"
      />
    </Box>
  )

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box className={cx(classes.filter)}>
          <TokenAssetGridFilter
            filterOption={filterOption}
            avFilterTypes={[
              FilterType.POPULAR,
              FilterType.ALL,
              FilterType.STABLECOINS,
              FilterType.LP,
            ]}
            handleFilterChange={(newOption) => setFilterOption(newOption)}
          />
        </Box>
        <Box className={cx(classes.assetList)}>
          {!mobile && (
            <TokenAssetGrid
              rows={vaults}
              columns={columns}
              disableSelectionOnClick
              disableColumnSelector
              disableColumnMenu
              hideFooterPagination
              rowHeight={64}
              autoHeight
              pageSize={7}
              components={{
                ColumnSortedDescendingIcon: SortedDescendingIcon,
                ColumnSortedAscendingIcon: SortedAscendingIcon,
              }}
            />
          )}
          {mobile &&
            vaults.map((row) => <TokenAssetCard key={row.id} row={row} />)}
        </Box>
      </Container>
    </Box>
  )
}

export default AssetSection
