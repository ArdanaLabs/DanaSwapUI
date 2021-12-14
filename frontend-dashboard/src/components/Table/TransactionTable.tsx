import React, { useEffect, useState } from "react"
import {
  Box,
  useMediaQuery,
  TableContainer,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
  TablePagination,
  CircularProgress,
} from "@material-ui/core"
import { makeStyles, useTheme, withStyles } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"
import { Button } from "components/Button"
import { usePoolTransactions } from "state/chart/hooks"
import { Any, Trade, Withdrawal, Deposit } from "config/txTypes"
import { findKeyFromObject, printCurrencyUSD } from "hooks"
import { useLocation } from "react-router-dom"
import { keys } from "lodash"

const StyledTableCellHead = withStyles(({ palette }) => ({
  root: {
    border: "unset",
  },
  head: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "13px",
    lineHeight: "300%",
    textAlign: "right",
    cursor: "pointer",
    color: palette.text.hint,
  },
  body: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "13px",
    lineHeight: "115%",
    textAlign: "left",
    cursor: "pointer",
    color: palette.text.hint,
  },
}))(TableCell)

const StyledTableCell = withStyles(({ palette }) => ({
  root: {
    border: "unset",
  },
  body: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "12px",
    lineHeight: "115%",
    textAlign: "right",
    color: palette.secondary.main,
    cursor: "pointer",
  },
}))(TableCell)

const StyledTablePagination = withStyles(({ palette }) => ({
  caption: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "12px",
    lineHeight: "115%",
    color: palette.secondary.main,
  },
  select: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "12px",
    lineHeight: "115%",
    color: palette.secondary.main,
  },
  menuItem: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "12px",
    lineHeight: "115%",
    color: palette.secondary.main,
  },
}))(TablePagination) as any

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  panel: {
    background: palette.background.paper,
    borderRadius: "10px",
    position: "relative",
    padding: "12px",
  },
  filter: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    margin: "20px 0",

    [breakpoints.down("xs")]: {
      "flexDirection": "column",
      "& > div": {
        width: "100%",
      },
    },
  },

  filterText: {
    "background":
      palette.type === "light"
        ? "#E7E7E7"
        : "linear-gradient(90deg, #3142A3 0%, #243183 100%)",
    "boxShadow":
      palette.type === "light"
        ? "unset"
        : "2px 2px 10px rgba(0, 0, 0, 0.1), inset 2px 2px 10px rgba(0, 0, 0, 0.1)",
    "fontSize": "11px",
    "fontWeight": 600,
    "lineHeight": "100%",
    "width": "500px",
    "padding": "15px 30px",
    "borderRadius": "20px",
    "color": palette.secondary.main,

    "& ::placeholder": {
      color: palette.secondary.main,
    },

    [breakpoints.down("xs")]: {
      flexDirection: "column",
      width: "100%",
    },
  },
  filterType: {
    background: palette.secondary.dark,
    color: palette.common.white,
    padding: "10px 15px",
    fontSize: "11px",
    lineHeight: "100%",
    margin: "10px",
  },

  active: {
    // background: palette.primary.light
  },
}))

const TransactionTable: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const { poolTransactions, getPoolTransactions } = usePoolTransactions()
  const location = useLocation()

  const columns = [
    "",
    "TOTAL VALUE",
    "TOKEN AMOUNT",
    "TOKEN AMOUNT",
    "ACCOUNT",
    "TIME",
  ]

  const [rows, setRows] = useState([])
  const [filter, setFilter] = useState({
    text: "",
    type: Any,
  })
  const [page, setPage] = useState(0)
  const [rowsPerPage, setRowsPerPage] = useState(10)
  const [loading, setLoading] = useState(true)

  const handleFilterChange = (event: any) => {
    setFilter({ ...filter, ...event })
    setPage(0)
  }
  const handleChangePage = (event: unknown, newPage: number) => {
    setPage(newPage)
  }
  const handleChangeRowsPerPage = (
    event: React.ChangeEvent<HTMLInputElement>
  ) => {
    setRowsPerPage(parseInt(event.target.value, 10))
    setPage(0)
  }

  useEffect(() => {
    const { poolName }: any = location.state
    getPoolTransactions(
      poolName,
      page * rowsPerPage,
      (page + 1) * rowsPerPage,
      filter.type
    )
    setLoading(true)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [page, rowsPerPage, filter.type])

  useEffect(() => {
    if (!poolTransactions) return

    let parsedRows = poolTransactions.map((row: any, i: number) => {
      const tag = findKeyFromObject(row, "tag")
      const navUSD = findKeyFromObject(row, "navUSD")
      const counterpartyAddress = findKeyFromObject(row, "counterpartyAddress")
      const created = findKeyFromObject(row, "created")
      const amounts = findKeyFromObject(row, "amounts")
      const purchasedAmount = findKeyFromObject(row, "purchasedAmount")
      const spentAmount = findKeyFromObject(row, "spentAmount")

      let action = ""
      let tokenAmountUSDC = 0
      let tokenAmount = 0

      switch (tag) {
        case "DepositTx":
          action = "Add USDC and ETH"
          // tokenAmountUSDC = amounts['USDC']  // true action
          tokenAmountUSDC = amounts[keys(amounts)[0]] // mock action
          // tokenAmount = amounts['ETH']  // true action
          tokenAmount = amounts[keys(amounts)[0]] // mock action
          break
        case "WithdrawalTx":
          action = "Remove USDC and ETH"
          // tokenAmountUSDC = amounts['USDC'] + ' USDC'  // true action
          tokenAmountUSDC = amounts[keys(amounts)[0]] // mock action
          // tokenAmount = amounts['ETH']  // true action
          tokenAmount = amounts[keys(amounts)[0]] // mock action
          break
        case "TradeTx":
          action = "Swap ETH for USDC"
          tokenAmountUSDC = purchasedAmount ?? spentAmount // mock action //  whether purchase or not
          tokenAmount = purchasedAmount ?? spentAmount // mock action //  whether purchase or not
          break
        default:
          break
      }
      return {
        action: action,
        navUSD: printCurrencyUSD(navUSD, 2),
        tokenAmountUSDC: `${printCurrencyUSD(tokenAmountUSDC, 2)} USDC`,
        tokenAmount: `${printCurrencyUSD(tokenAmount, 2)} ETH`,
        counterpartyAddress: counterpartyAddress,
        created: created,
      }
    })

    setRows(parsedRows)
    setLoading(false)
  }, [poolTransactions])

  return (
    <Box>
      <Box className={cx(classes.filter)}>
        <Box textAlign="center" mt={mobile ? "20px" : "0px"}>
          <Button
            variant="contained"
            onClick={() => {
              handleFilterChange({ type: Any })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === Any,
            })}
          >
            ALL
          </Button>
          <Button
            variant="contained"
            onClick={() => {
              handleFilterChange({ type: Trade })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === Trade,
            })}
          >
            SWAPS
          </Button>
          <Button
            variant="contained"
            onClick={() => {
              handleFilterChange({ type: Deposit })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === Deposit,
            })}
          >
            ADDS
          </Button>
          <Button
            variant="contained"
            onClick={() => {
              handleFilterChange({ type: Withdrawal })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === Withdrawal,
            })}
          >
            REMOVES
          </Button>
        </Box>
      </Box>
      <Box className={cx(classes.panel)}>
        {rows && (
          <>
            <TableContainer>
              <Table aria-label="simple table">
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
                    <TableRow key={i} hover>
                      <StyledTableCellHead>{row.action}</StyledTableCellHead>
                      <StyledTableCell>{row.navUSD}</StyledTableCell>
                      <StyledTableCell>{row.tokenAmountUSDC}</StyledTableCell>
                      <StyledTableCell>{row.tokenAmount}</StyledTableCell>
                      <StyledTableCell>
                        {row.counterpartyAddress}
                      </StyledTableCell>
                      <StyledTableCell>{row.created}</StyledTableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </TableContainer>
            <StyledTablePagination
              rowsPerPageOptions={[5, 10, 25]}
              component="div"
              count={100} //  mock
              rowsPerPage={rowsPerPage}
              page={page}
              onPageChange={handleChangePage}
              onRowsPerPageChange={handleChangeRowsPerPage}
            />
          </>
        )}
        {loading && (
          <Box
            position="absolute"
            top={0}
            left={0}
            width="100%"
            height="100%"
            display="flex"
            justifyContent="center"
            alignItems="center"
          >
            <CircularProgress />
          </Box>
        )}
      </Box>
    </Box>
  )
}

export default TransactionTable
