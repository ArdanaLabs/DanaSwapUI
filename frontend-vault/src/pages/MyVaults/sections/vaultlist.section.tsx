import { Box, Container, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import _ from "lodash"
import {
  TokenAssetGrid,
  VaultCard,
  TokenAssetGridFilter,
  VaultButton,
} from "components"
import {
  FilterOption,
  FilterType,
} from "components/DataGrid/TokenAssetGridFilter"
import React, { useMemo, useState } from "react"
import { useIsDarkMode } from "state/user/hooks"
import { useWallet } from "state/wallet/hooks"

import {
  GridCellParams,
  GridColDef,
  GridValueGetterParams,
} from "@material-ui/data-grid"
import { currencyFormatter, percentageFormatter } from "hooks"
import { useUiModal } from "state/ui/hooks"
import { MyVaultInfo } from "state/wallet/types"
import { ReactComponent as ChevDownIcon } from "assets/image/svgs/chev-down.svg"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    margin: "50px 0px",
  },
  sortIcon: {
    [`& path`]: {
      fill: palette.primary.main,
    },
  },
  uppercase: {
    textTransform: "uppercase",
  },
  flip: {
    transform: "rotateX(180deg)",
  },
}))

const VaultListSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const { myVaults } = useWallet()
  const { toggleModal } = useUiModal()

  const [filterOption, setFilterOption] = useState<FilterOption>({
    filterType: FilterType.YOUR,
    keyword: "",
  })

  const filteredVaults: MyVaultInfo[] = useMemo(() => {
    let filteredByKeyword: MyVaultInfo[] = myVaults.filter(
      (vault: MyVaultInfo) =>
        _.isEmpty(filterOption.keyword) ||
        vault.asset.indexOf(filterOption.keyword) !== -1
    )
    switch (filterOption.filterType) {
      case FilterType.STABLECOINS:
        filteredByKeyword = filteredByKeyword.filter(
          (vault: MyVaultInfo) => vault.isStableCoin
        )
        break
      case FilterType.LP:
        filteredByKeyword = filteredByKeyword.filter(
          (vault: MyVaultInfo) => vault.isLP
        )
        break
      default:
        break
    }
    return filteredByKeyword
  }, [myVaults, filterOption])

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
      field: "id",
      headerName: "Vault ID",
      sortable: false,
      flex: 1,
      valueGetter: (params: GridValueGetterParams) => "#" + params.value,
    },
    {
      field: "locked",
      headerName: "Liquidation Price",
      type: "number",
      flex: 1,
      align: "left",
      headerAlign: "left",
      valueGetter: (params: GridValueGetterParams) =>
        currencyFormatter(Number(params.value)),
    },
    {
      field: "collRatio",
      headerName: "Colt Ratio",
      type: "number",
      flex: 1,
      align: "left",
      headerAlign: "left",
      valueGetter: (params: GridValueGetterParams) =>
        percentageFormatter(Number(params.value), 0),
    },
    {
      field: "debt",
      headerName: "DAI Debt",
      type: "number",
      flex: 1,
      align: "left",
      headerAlign: "left",
      valueGetter: (params: GridValueGetterParams) => Number(params.value),
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
            Manage Vault
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
    <Box className={cx(classes.root)}>
      <Container>
        <Box>
          <TokenAssetGridFilter
            filterOption={filterOption}
            avFilterTypes={[
              FilterType.YOUR,
              FilterType.STABLECOINS,
              FilterType.LP,
            ]}
            handleFilterChange={(newOption) => setFilterOption(newOption)}
          />
        </Box>
        <Box>
          {!mobile && (
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
          )}
          {mobile &&
            filteredVaults.map((row: MyVaultInfo) => (
              <VaultCard key={row.id} row={row} />
            ))}
        </Box>
      </Container>
    </Box>
  )
}

export default VaultListSection
