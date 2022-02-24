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

import * as E from "fp-ts/Either"
import * as O from "fp-ts/Option"
import * as ROM from "fp-ts/ReadonlyMap"
import * as RemoteData from "fp-ts-remote-data"

import * as Asset from "Data/Asset"
import { AssetQuantity } from "Data/Unit"
import { TransactionType } from "Data/Chart/TransactionType"
import { FetchDecodeError, FetchDecodeResult } from "Data/FetchDecode"
import * as PoolSetName from "Data/Pool/PoolSetName"
import * as Transaction from "Data/Transaction"
import * as Transfer from "Data/Transfer"
import * as Trade from "Data/Trade"
import * as Theme from "Data/User/Theme"

import { printAssetQuantity, printDate } from "hooks"
import { useUserTheme } from "state/user/hooks"
import { Button } from "components/Button"
import { fetchPoolTransactions } from "state/chart/hooks"

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
    minHeight: "72px",
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
    textTransform: "uppercase",
  },

  active: {
    // background: palette.primary.light
  },
}))

export type Props = {
  poolSet: PoolSetName.Type
}

export const TransactionTable: React.FC<Props> = ({
  poolSet,
}: Props): JSX.Element => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

  const columns = [
    "",
    "Total Value",
    "Token Amount",
    "Token Amount",
    "Account",
    "Time",
  ]

  const [rows, setRows] = useState<
    RemoteData.RemoteData<FetchDecodeError, Transaction.WithTotalValue[]>
  >(RemoteData.pending)
  const [filter, setFilter] = useState({
    text: "",
    type: TransactionType.Any,
  })
  const [page, setPage] = useState(0)
  const [rowsPerPage, setRowsPerPage] = useState(10)

  const handleFilterChange = (event: object) => {
    setFilter({ ...filter, ...event })
    setPage(0)
  }
  const handleChangePage = (_event: object, newPage: number) => {
    setPage(newPage)
  }
  const handleChangeRowsPerPage = (
    event: React.ChangeEvent<HTMLInputElement>
  ) => {
    setRowsPerPage(parseInt(event.target.value, 10))
    setPage(0)
  }

  useEffect(() => {
    fetchPoolTransactions(
      poolSet,
      page * rowsPerPage,
      (page + 1) * rowsPerPage,
      filter.type
    )().then(
      (transactions: FetchDecodeResult<Transaction.WithTotalValue[]>): void => {
        E.isLeft(transactions) &&
          console.error("Fetch processing error:", transactions.left)
        setRows(RemoteData.fromEither(transactions))
      }
    )

    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [page, rowsPerPage, filter.type])

  /* TODO: we need to move from the below Mock function to something like this…
  const getPairQuantity =
    (as: Transfer.Amounts.Type) =>
    (
      assets: [Asset, Asset]
    ): [O.Option<[Asset, AssetQuantity.Type]>, O.Option<[Asset, AssetQuantity.Type]>] => {
      function lookup(k: Asset): O.Option<[Asset, AssetQuantity.Type]> {
        return O.map((aq: AssetQuantity.Type): [Asset, AssetQuantity.Type] => [k, aq])(
          ROM.lookup(eqAsset)(k)(as)
        )
      }
      return [lookup(assets[0]), lookup(assets[1])]
    }
  */

  const getPairQuantityMock = (
    as: Transfer.Amounts.Type
  ): [O.Option<AssetQuantity.Type>, O.Option<AssetQuantity.Type>] => {
    const ks: ReadonlyArray<Asset.Type> = ROM.keys(Asset.Ord)(as)
    return [O.fromNullable(as.get(ks[0])), O.fromNullable(as.get(ks[1]))]
  }

  function renderTransaction(
    { transaction, navUSD }: Transaction.WithTotalValue,
    index: number
  ): JSX.Element | undefined {
    switch (transaction._tag) {
      case "Trade":
        const t: Trade.Type = transaction.trade
        // purchasedAmount ?? spentAmount
        return (
          <TableRow key={index} hover>
            <StyledTableCellHead>Swap USDC and ETH</StyledTableCellHead>
            <StyledTableCell>{t.purchasedAmount}</StyledTableCell>
            <StyledTableCell></StyledTableCell>
            <StyledTableCell>{t.purchasedAmount}</StyledTableCell>
            <StyledTableCell>{t.counterpartyAddress}</StyledTableCell>
            <StyledTableCell>{printDate(t.created)}</StyledTableCell>
          </TableRow>
        )

      case "Deposit":
        const d: Transfer.Type = transaction.deposit
        //key = d.counterpartyAddress
        // tokenAmountUSDC = amounts['USDC']  // true action
        // tokenAmount = amounts['ETH']  // true action
        const [daqx, daqy] = getPairQuantityMock(d.amounts)
        return (
          <TableRow key={index} hover>
            <StyledTableCellHead>Add USDC and ETH</StyledTableCellHead>
            <StyledTableCell>{navUSD}</StyledTableCell>
            <StyledTableCell>
              {O.fold(
                () => "",
                (aq: AssetQuantity.Type): string =>
                  printAssetQuantity(Asset.iso.wrap("USDC"), aq)
              )(daqx)}
            </StyledTableCell>
            <StyledTableCell>
              {O.fold(
                () => "",
                (aq: AssetQuantity.Type): string =>
                  printAssetQuantity(Asset.iso.wrap("ETH"), aq)
              )(daqy)}
            </StyledTableCell>
            <StyledTableCell>{d.counterpartyAddress}</StyledTableCell>
            <StyledTableCell>{printDate(d.created)}</StyledTableCell>
          </TableRow>
        )

      case "Withdrawal":
        const w: Transfer.Type = transaction.withdrawal
        //key = w.counterpartyAddress
        // tokenAmountUSDC = amounts['USDC']  // true action
        // tokenAmount = amounts['ETH']  // true action
        const [waqx, waqy] = getPairQuantityMock(w.amounts)
        return (
          <TableRow key={index} hover>
            <StyledTableCellHead>Remove USDC and ETH</StyledTableCellHead>
            <StyledTableCell>{navUSD}</StyledTableCell>
            <StyledTableCell>
              {O.fold(
                () => "",
                (aq: AssetQuantity.Type): string =>
                  printAssetQuantity(Asset.iso.wrap("USDC"), aq)
              )(waqx)}
            </StyledTableCell>
            <StyledTableCell>
              {O.fold(
                () => "",
                (aq: AssetQuantity.Type): string =>
                  printAssetQuantity(Asset.iso.wrap("ETH"), aq)
              )(waqy)}
            </StyledTableCell>
            <StyledTableCell>{w.counterpartyAddress}</StyledTableCell>
            <StyledTableCell>{printDate(w.created)}</StyledTableCell>
          </TableRow>
        )
    }
  }

  function renderTransactions(
    ts: RemoteData.RemoteData<FetchDecodeError, Transaction.WithTotalValue[]>
  ): JSX.Element {
    switch (ts._tag) {
      case "Success":
        return (
          <>
            <TableContainer>
              <Table aria-label="simple table">
                <TableHead>
                  <TableRow>
                    {columns.map((column: string, index: number) => (
                      <StyledTableCellHead key={`${column}-${index}`}>
                        {/* TODO: text-transform: upperase */}
                        {column}
                      </StyledTableCellHead>
                    ))}
                  </TableRow>
                </TableHead>
                <TableBody>{ts.success.map(renderTransaction)}</TableBody>
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
        )
      case "Pending":
        return (
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
        )
      case "Failure":
        return (
          <details>
            <summary>TODO: Error</summary>
            <pre>{JSON.stringify(ts.failure, null, 2)}</pre>
          </details>
        )
    }
  }

  return (
    <Box>
      <Box className={cx(classes.filter)}>
        <Box textAlign="center" mt={mobile ? "20px" : "0px"}>
          {[
            [TransactionType.Any, "Any"],
            [TransactionType.Trade, "Swap"],
            [TransactionType.Deposit, "Adds"],
            [TransactionType.Withdrawal, "Removes"],
          ].map(([transactionType, label]) => {
            return (
              <Button
                variant="contained"
                onClick={() => {
                  handleFilterChange({ type: transactionType })
                }}
                className={cx(classes.filterType, {
                  [classes.active]: filter.type === transactionType,
                })}
                key={label}
              >
                {label}
              </Button>
            )
          })}
        </Box>
      </Box>
      <Box className={cx(classes.panel)}>{renderTransactions(rows)}</Box>
    </Box>
  )
}

export default TransactionTable
