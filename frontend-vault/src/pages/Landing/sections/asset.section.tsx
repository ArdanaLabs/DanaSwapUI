import React, { useState } from 'react'
import { Box, Container, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import KeyboardArrowDownIcon from '@material-ui/icons/KeyboardArrowDown'
import KeyboardArrowUpIcon from '@material-ui/icons/KeyboardArrowUp'
import cx from 'classnames'
import { useIsDarkMode } from 'state/user/hooks'
import { TokenAssetGrid, TokenAssetGridFilter, TokenAssetCard, VaultButton } from 'components'
import {
  GridCellParams,
  GridColDef,
  GridValueGetterParams
} from '@material-ui/data-grid'
import {
  FilterOption,
  FilterType
} from 'components/DataGrid/TokenAssetGridFilter'

import COIN1 from 'assets/image/COIN1.png'

const rows = [
  {
    id: 0,
    asset: 'Wrapped Bitcoin1',
    type: 'WBTC-A',
    dUSD: '29.36M',
    stabilityFee: 2,
    minColl: 150,
    assetIcon: COIN1
  },
  {
    id: 17,
    asset: 'Wrapped Bitcoin2',
    type: 'WBTC-A',
    dUSD: '29.36M',
    stabilityFee: 2,
    minColl: 150,
    assetIcon: COIN1
  },
  {
    id: 18,
    asset: 'Wrapped Bitcoin3',
    type: 'WBTC-A',
    dUSD: '29.36M',
    stabilityFee: 1,
    minColl: 150,
    assetIcon: COIN1
  },
  {
    id: 19,
    asset: 'Wrapped Bitcoin4',
    type: 'WBTC-A',
    dUSD: '29.36M',
    stabilityFee: 3,
    minColl: 150,
    assetIcon: COIN1
  },
  {
    id: 110,
    asset: 'Wrapped Bitcoin5',
    type: 'WBTC-A',
    dUSD: '29.36M',
    stabilityFee: 1,
    minColl: 150,
    assetIcon: COIN1
  },
  {
    id: 120,
    asset: 'Wrapped Bitcoin6',
    type: 'WBTC-A',
    dUSD: '29.36M',
    stabilityFee: 20,
    minColl: 150,
    assetIcon: COIN1
  },
  {
    id: 130,
    asset: 'Wrapped Bitcoin7',
    type: 'WBTC-A',
    dUSD: '29.36M',
    stabilityFee: 2,
    minColl: 150,
    assetIcon: COIN1
  }
]

const columns: GridColDef[] = [
  {
    field: 'asset',
    headerName: 'Asset',
    sortable: false,
    flex: 2,
    renderCell: (params: GridCellParams) => {
      const assetIcon = params.getValue(params.id, 'assetIcon')
      return (
        <Box display='flex' alignItems='center'>
          <img src={assetIcon?.toString()} alt='' />
          <Box pl='15px'>{params.value}</Box>
        </Box>
      )
    }
  },
  {
    field: 'type',
    headerName: 'Type',
    sortable: false,
    flex: 1
  },
  {
    field: 'dUSD',
    headerName: 'dUSD Available',
    flex: 1
  },
  {
    field: 'stabilityFee',
    headerName: 'Stability Fee',
    type: 'number',
    flex: 1,
    align: 'left',
    headerAlign: 'left',
    valueGetter: (params: GridValueGetterParams) =>
      Number(params.value).toFixed(2) + '%'
  },
  {
    field: 'minColl',
    headerName: 'Min Coll. Ratio',
    type: 'number',
    flex: 1,
    align: 'left',
    headerAlign: 'left',
    valueGetter: (params: GridValueGetterParams) => params.value + '%'
  },
  {
    field: 'action',
    headerName: ' ',
    sortable: false,
    width: 200,
    align: 'center',
    renderCell: () => <VaultButton>Open Vault</VaultButton>
  }
]

export function SortedDescendingIcon () {
  return <KeyboardArrowDownIcon color='primary' />
}

export function SortedAscendingIcon () {
  return <KeyboardArrowUpIcon color='primary' />
}

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {},
  filter: {},
  assetList: {
    width: '100%'
  },
}))

const AssetSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })
  const [filterOption, setFilterOption] = useState<FilterOption>({
    filterType: FilterType.POPULAR,
    keyword: ''
  })

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box className={cx(classes.filter)}>
          <TokenAssetGridFilter
            filterOption={filterOption}
            handleFilterChange={newOption => setFilterOption(newOption)}
          />
        </Box>
        <Box className={cx(classes.assetList)}>
          {!mobile && (
            <TokenAssetGrid
              rows={rows}
              columns={columns}
              disableSelectionOnClick
              disableColumnSelector
              disableColumnMenu
              hideFooterPagination
              rowHeight={70}
              autoHeight
              pageSize={7}
              components={{
                ColumnSortedDescendingIcon: SortedDescendingIcon,
                ColumnSortedAscendingIcon: SortedAscendingIcon
              }}
            />
          )}
          {mobile && rows.map((row) => (
            <TokenAssetCard
              key={row.id}
              id={row.id}
              asset={row.asset}
              type={row.type}
              dUSD={row.dUSD}
              stabilityFee={row.stabilityFee}
              minColl={row.minColl}
              assetIcon={row.assetIcon}
            />
          ))}
        </Box>
      </Container>
    </Box>
  )
}

export default AssetSection
