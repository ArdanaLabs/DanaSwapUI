import React, { useMemo, useState } from "react"
import _ from "lodash"
import cx from "classnames"
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
} from "@mui/x-data-grid"
import {
  FilterOption,
  FilterType,
} from "components/DataGrid/TokenAssetGridFilter"

import { useUiModal } from "state/ui/hooks"
import { useVault } from "state/vault/hooks"
import { numberFormatter, percentageFormatter } from "hooks/formatter"
import { ReactComponent as ChevDownIcon } from "assets/image/svgs/chev-down.svg"
import { VaultInfo } from "state/vault/types"

import { makeStyles } from "@mui/styles"
import {
  Box,
  Container,
  Theme,
  Typography,
  useMediaQuery,
  useTheme,
} from "@mui/material"

const useStyles = makeStyles((theme: Theme) => ({
  root: {},
  filter: {},
  assetList: {
    width: "100%",
  },
  sortIcon: {
    [`& path`]: {
      fill: theme.palette.primary.main,
    },
  },
  uppercase: {
    textTransform: "uppercase",
  },
  flip: {
    transform: "rotateX(180deg)",
  },
  vaultButton: {
    color: theme.palette.primary.main,
    textTransform: "uppercase",
    lineHeight: "100%",
  },
}))

const AssetSection: React.FC = () => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down("sm"))
  const classes = useStyles(theme)
  const [filterOption, setFilterOption] = useState<FilterOption>({
    filterType: FilterType.Popular,
    keyword: "",
  })

  const { toggleModal } = useUiModal()
  const { vaults } = useVault()

  const filteredVaults: VaultInfo[] = useMemo(() => {
    let filteredByKeyword: VaultInfo[] = vaults.filter(
      (vault: VaultInfo) =>
        _.isEmpty(filterOption.keyword) ||
        vault.asset.indexOf(filterOption.keyword) !== -1
    )
    switch (filterOption.filterType) {
      case FilterType.Popular:
        filteredByKeyword = filteredByKeyword.filter(
          (vault: VaultInfo) => vault.isPopular
        )
        break
      case FilterType.Stablecoins:
        filteredByKeyword = filteredByKeyword.filter(
          (vault: VaultInfo) => vault.isStableCoin
        )
        break
      case FilterType.LiquidityPool:
        filteredByKeyword = filteredByKeyword.filter(
          (vault: VaultInfo) => vault.isLP
        )
        break
      default:
        break
    }
    return filteredByKeyword
  }, [vaults, filterOption])

  const columns: GridColDef[] = [
    {
      field: "asset",
      headerName: "Asset",
      sortable: false,
      flex: 2,
      renderCell: (params: GridCellParams) => {
        let assetLogo = null
        try {
          assetLogo = require(`assets/image/coins/${params.value}.svg`).default
        } catch (e) {}
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
            <Typography variant="h3" className={classes.vaultButton}>
              Open Vault
            </Typography>
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
      <ChevDownIcon className={cx(classes.sortIcon, classes.flip)} />
    </Box>
  )

  const SortedAscendingIcon = () => (
    <Box display="flex" justifyContent="center" alignItems="center">
      <ChevDownIcon className={classes.sortIcon} />
    </Box>
  )

  return (
    <Box className={classes.root}>
      <Container>
        <Box className={classes.filter}>
          <TokenAssetGridFilter
            filterOption={filterOption}
            avFilterTypes={[
              FilterType.Popular,
              FilterType.All,
              FilterType.Stablecoins,
              FilterType.LiquidityPool,
            ]}
            handleFilterChange={(newOption) => setFilterOption(newOption)}
          />
        </Box>
        <Box className={classes.assetList}>
          {!mobile ? (
            <TokenAssetGrid
              rows={filteredVaults}
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
          ) : (
            filteredVaults.map((row) => (
              <TokenAssetCard key={row.id} row={row} />
            ))
          )}
        </Box>
      </Container>
    </Box>
  )
}

export default AssetSection
