import React, { useState } from 'react'
import {
  Box,
  useMediaQuery,
  TableContainer,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
} from '@material-ui/core'
import { makeStyles, useTheme, withStyles } from '@material-ui/core/styles'
import cx from 'classnames'
import { useIsDarkMode } from 'state/user/hooks'
import { Button } from 'components/Button'

const FILTER_ALL = 0
const FILTER_SWAP = 1
const FILTER_ADD = 2
const FILTER_REMOVE = 3

const StyledTableCellHead = withStyles(({ palette }) => ({
  root: {
    border: 'unset',
  },
  head: {
    fontFamily: 'Museo Sans',
    fontStyle: 'normal',
    fontWeight: 700,
    fontSize: '13px',
    lineHeight: '300%',
    textAlign: 'right',
    cursor: 'pointer',
    color: palette.text.hint
  },
  body: {
    fontFamily: 'Museo Sans',
    fontStyle: 'normal',
    fontWeight: 700,
    fontSize: '13px',
    lineHeight: '115%',
    textAlign: 'right',
    cursor: 'pointer',
    color: palette.text.hint
  }
}))(TableCell)

const StyledTableCell = withStyles(({ palette }) => ({
  root: {
    border: 'unset',
  },
  body: {
    fontFamily: 'Museo Sans',
    fontStyle: 'normal',
    fontWeight: 700,
    fontSize: '12px',
    lineHeight: '115%',
    textAlign: 'right',
    color: palette.secondary.main,
    cursor: 'pointer'
  }
}))(TableCell)

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  panel: {
    background: palette.background.paper,
    borderRadius: '10px',
    padding: '12px'
  },
  filter: {
    display: 'flex',
    justifyContent: 'space-between',
    alignItems: 'center',
    margin: '20px 0',

    [breakpoints.down('xs')]: {
      flexDirection: 'column',
      '& > div': {
        width: '100%'
      }
    }
  },

  filterText: {
    background:
      palette.type === 'light'
        ? '#E7E7E7'
        : 'linear-gradient(90deg, #3142A3 0%, #243183 100%)',
    boxShadow:
      palette.type === 'light'
        ? 'unset'
        : '2px 2px 10px rgba(0, 0, 0, 0.1), inset 2px 2px 10px rgba(0, 0, 0, 0.1)',
    fontSize: '11px',
    fontWeight: 600,
    lineHeight: '100%',
    width: '500px',
    padding: '15px 30px',
    borderRadius: '20px',
    color: palette.secondary.main,

    '& ::placeholder': {
      color: palette.secondary.main
    },

    [breakpoints.down('xs')]: {
      flexDirection: 'column',
      width: '100%'
    }
  },
  filterType: {
    background: palette.secondary.dark,
    color: palette.common.white,
    padding: '10px 15px',
    fontSize: '11px',
    lineHeight: '100%',
    margin: '10px'
  },

  active: {
    // background: palette.primary.light
  }
}))

const TransactionTable: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })

  const columns = [
    '',
    'TOTAL VALUE',
    'TOKEN AMOUNT',
    'TOKEN AMOUNT',
    'ACCOUNT',
    'TIME'
  ]
  const rows = [
    {
      action: "Add USDC and ETH",
      totalValue: "$1.98k",
      tokenAmountUSDC: "0.93K USDC",
      tokenAmount: "0.3 DANA",
      account: "0x362C...2f25",
      time: "47 minutes ago",
    },
    {
      action: "Add USDC and ETH",
      totalValue: "$1.98k",
      tokenAmountUSDC: "0.93K USDC",
      tokenAmount: "0.3 DANA",
      account: "0x362C...2f25",
      time: "47 minutes ago",
    },
    {
      action: "Add USDC and ETH",
      totalValue: "$1.98k",
      tokenAmountUSDC: "0.93K USDC",
      tokenAmount: "0.3 DANA",
      account: "0x362C...2f25",
      time: "47 minutes ago",
    },
  ]

  const [filter, setFilter] = useState({
    text: '',
    type: FILTER_ALL
  })

  const onFilterChange = (event: any) => {
    setFilter({ ...filter, ...event })
  }
  return (
    <Box>
      <Box className={cx(classes.filter)}>
        <Box textAlign='center' mt={mobile ? '20px' : '0px'}>
          <Button
            variant='contained'
            onClick={() => {
              onFilterChange({ type: FILTER_ALL })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_ALL
            })}
          >
            ALL
          </Button>
          <Button
            variant='contained'
            onClick={() => {
              onFilterChange({ type: FILTER_SWAP })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_SWAP
            })}
          >
            SWAPS
          </Button>
          <Button
            variant='contained'
            onClick={() => {
              onFilterChange({ type: FILTER_ADD })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_ADD
            })}
          >
            ADDS
          </Button>
          <Button
            variant='contained'
            onClick={() => {
              onFilterChange({ type: FILTER_REMOVE })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_REMOVE
            })}
          >
            REMOVES
          </Button>
        </Box>
      </Box>
      <TableContainer component={Box} className={cx(classes.panel)}>
        <Table aria-label='simple table'>
          <TableHead>
            <TableRow>
              {columns.map((column: any, i: any) => (
                <StyledTableCellHead key={i}>
                  {column}
                </StyledTableCellHead>
              ))}
            </TableRow>
          </TableHead>
          <TableBody>
            {rows.map((row: any, i: any) => (
              <TableRow key={i}>
                <StyledTableCellHead>{row.action}</StyledTableCellHead>
                <StyledTableCell>{row.totalValue}</StyledTableCell>
                <StyledTableCell>{row.tokenAmountUSDC}</StyledTableCell>
                <StyledTableCell>{row.tokenAmount}</StyledTableCell>
                <StyledTableCell>{row.account}</StyledTableCell>
                <StyledTableCell>{row.time}</StyledTableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
      {/* <TablePagination
          rowsPerPageOptions={[5, 10, 25]}
          component="div"
          count={rows.length}
          rowsPerPage={rowsPerPage}
          page={page}
          onPageChange={handleChangePage}
          onRowsPerPageChange={handleChangeRowsPerPage}
        /> */}
    </Box>
  )
}

export default TransactionTable
