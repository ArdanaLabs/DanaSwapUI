import React from "react"
import cx from "classnames"
import {
  Box,
  useMediaQuery,
  TableContainer,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme, withStyles } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"
import { useHistory } from "react-router-dom"

import * as O from "fp-ts/Option"
import * as ROM from "fp-ts/ReadonlyMap"

import * as PoolSetName from "Data/Pool/PoolSetName"
import { PoolStats } from "Data/Stats/PoolStats"
import { Percent, USD } from "Data/Unit"

import { printCurrencyUSD, printPercentage } from "hooks"

const StyledTableRow = withStyles(({ palette }) => ({
  root: {
    [`& > th`]: {
      border: `1px solid ${palette.secondary.main}88`,
      borderTopLeftRadius: "100px",
      borderBottomLeftRadius: "100px",
      borderRight: "unset",
    },
    [`& > td`]: {
      borderTop: `1px solid ${palette.secondary.main}88`,
      borderBottom: `1px solid ${palette.secondary.main}88`,
    },
    [`& > td:last-child`]: {
      borderRight: `1px solid ${palette.secondary.main}88`,
      borderTopRightRadius: "100px",
      borderBottomRightRadius: "100px",
    },
  },
  head: {},
}))(TableRow)

const StyledTableCell = withStyles(({ palette, breakpoints }) => ({
  root: {
    border: `unset`,

    [breakpoints.down("xs")]: {
      padding: 5,
    },
  },
  head: {
    [`& span`]: {
      color: palette.secondary.main,
      textTransform: "uppercase",
      fontWeight: 900,
      whiteSpace: "nowrap",
    },
  },
  body: {
    [`& span`]: {
      color: palette.primary.main,
      fontWeight: 300,
      whiteSpace: "nowrap",

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
  },
}))(TableCell)

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {},
  AssetLogo: {
    [`& > img`]: {
      width: 40,
      height: 40,

      [breakpoints.down("xs")]: {
        width: 20,
        height: 20,
      },
    },
  },
  TableRoot: {
    borderCollapse: "separate",

    [breakpoints.down("xs")]: {
      borderSpacing: "0 10px",
    },
  },
}))

export interface PoolsGridProps {
  rows: ReadonlyMap<PoolSetName.Type, PoolStats>
}

const PoolsGrid: React.FC<PoolsGridProps> = ({ rows }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const history = useHistory()

  const columns: string[] = [
    "Pool",
    // "Liquidity",
    "Base APY",
    "Rewards APY",
    "Volume",
    "APY",
  ]

  function handleRowClick(poolSetName: PoolSetName.Type): void {
    history.push({
      pathname: `/spec/${PoolSetName.iso.unwrap(poolSetName)}`,
    })
  }

  function renderTableRow(
    poolName: PoolSetName.Type,
    ps: PoolStats
  ): JSX.Element {
    // TODO: use SVG sprite, like `coins.svg#${poolName}`
    // const icon = require(`assets/coins/${poolName}.png`).default
    const poolSetNameStr: string = PoolSetName.iso.unwrap(poolName)
    const icon = require(`assets/coins/BTC.png`).default
    return (
      <StyledTableRow
        hover
        key={poolSetNameStr}
        onClick={() => handleRowClick(poolName)}
      >
        {/* POOL */}
        <StyledTableCell component="th" scope="row" style={{ minWidth: 200 }}>
          <Box display="flex" alignItems="center">
            <Box className={classes.AssetLogo} display={"flex"} mr={2}>
              <img src={icon} alt={poolSetNameStr} />
            </Box>
            <Box display={"flex"} flexDirection={"column"}>
              <Typography
                variant="h4"
                component="span"
                className={cx("bolder", "smaller")}
              >
                {poolName}
              </Typography>
              <Typography
                variant="h4"
                component="span"
                className={cx("smaller")}
              >
                DAI + USDC + USDT + sUSD
              </Typography>
            </Box>
          </Box>
        </StyledTableCell>
        {/* Liquidity */}
        {/* <StyledTableCell>
          {O.fold(() => "", printCurrencyUSD)(ps.navUSD)}
        </StyledTableCell> */}
        {/* Base APY */}
        <StyledTableCell>
          <Typography variant="h4" component="span" className={cx("bolder")}>
            {printPercentage(
              O.getOrElse(() => Percent.iso.wrap(0))(ps.recentDailyAPYPercent)
            )}
          </Typography>
        </StyledTableCell>
        {/* Rewards APY */}
        <StyledTableCell style={{ minWidth: 200 }}>
          <Typography variant="h4" component="span">
            {/* {poolRewardAPYs[i]} */}
            {`+4.30% → 10.76% DANA + 1.13% BTC`}
          </Typography>
        </StyledTableCell>
        {/* VOLUME */}
        <StyledTableCell>
          <Typography variant="h4" component="span" className={cx("bolder")}>
            {printCurrencyUSD(
              O.getOrElse(() => USD.iso.wrap(0))(ps.recentDailyVolumeUSD.trade)
            )}
          </Typography>
        </StyledTableCell>
        {/* APY */}
        <StyledTableCell>
          <Typography variant="h4" component="span" className={cx("bolder")}>
            {printPercentage(
              O.getOrElse(() => Percent.iso.wrap(0))(ps.recentDailyAPYPercent)
            )}
          </Typography>
        </StyledTableCell>
      </StyledTableRow>
    )
  }

  function renderTable(
    poolStatsMap: ReadonlyMap<PoolSetName.Type, PoolStats>
  ): JSX.Element {
    return (
      <TableContainer component={Box}>
        <Table aria-label="simple table" className={classes.TableRoot}>
          <TableHead>
            <TableRow>
              {columns.map((column: string, index: number) => {
                return (
                  <StyledTableCell key={index}>
                    <Typography variant="h5" component="span">
                      {column}
                    </Typography>
                  </StyledTableCell>
                )
              })}
            </TableRow>
          </TableHead>
          <TableBody>
            {/* TODO: is `collect` the correct, performant solution? */}
            {ROM.collect(PoolSetName.Ord)(renderTableRow)(poolStatsMap)}
          </TableBody>
        </Table>
      </TableContainer>
    )
  }

  return <Box className={classes.root}>{renderTable(rows)}</Box>
}

export default PoolsGrid
