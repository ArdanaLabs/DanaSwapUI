import React, { useState } from "react"
import {
  Box,
  useMediaQuery,
  TableContainer,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
  TableFooter,
} from "@material-ui/core"
import { makeStyles, useTheme, withStyles } from "@material-ui/core/styles"
import cx from "classnames"
import { useHistory } from "react-router-dom"

import * as O from "fp-ts/Option"
import * as ROM from "fp-ts/ReadonlyMap"
import { RemoteData } from "fp-ts-remote-data"

import { FetchDecodeError } from "Data/FetchDecode"
import * as PoolSetName from "Data/Pool/PoolSetName"
import { PoolStats } from "Data/Stats/PoolStats"
import { Percent, USD } from "Data/Unit"
import * as Theme from "Data/User/Theme"

import { usePoolStats } from "state/home/hooks"
import { printCurrencyUSD, printPercentage } from "hooks"
import { useUserTheme } from "state/user/hooks"
import { SearchInput } from "components/Input"
import { Button } from "components/Button"

enum FilterOn {
  StableCoins = 0,
  DigitalAssets = 1,
}

const StyledTableCell = withStyles(({ palette }) => ({
  root: {
    borderBottom: "1px solid #E5E5E5",
  },
  head: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "14px",
    lineHeight: "300%",
    textAlign: "center",
    cursor: "pointer",
    color: palette.secondary.main,
  },
  body: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: "bold",
    fontSize: "14px",
    lineHeight: "115%",
    textAlign: "center",
    color: palette.secondary.main,
    cursor: "pointer",
  },
  footer: {
    "width": "100%",
    "height": 30,
    "textAlign": "center",

    "& span": {
      "fontFamily": "Museo Sans",
      "fontStyle": "normal",
      "fontWeight": "bold",
      "fontSize": "16px",
      "lineHeight": "115%",
      "cursor": "pointer",
      "color": palette.secondary.main,

      "&:hover": {
        color: "gray",
      },
    },
  },
}))(TableCell)

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  panel: {
    background: palette.background.paper,
    borderRadius: "10px",
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
    padding: "15px",
    fontSize: "11px",
    lineHeight: "100%",
    marginLeft: "10px",
    width: "150px",

    [breakpoints.down("xs")]: {
      width: "auto",
    },
  },

  active: {
    background: palette.primary.light,
  },
}))

export interface PoolsPanelProps {
  overview?: boolean
}

const PoolsPanel: React.FC<PoolsPanelProps> = ({ overview = false }) => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })
  const history = useHistory()

  const columns: string[] = [
    "Pool",
    "Liquidity",
    "Base APY",
    "Rewards APY",
    "Volume",
    "APY",
  ]

  const poolStats: RemoteData<
    FetchDecodeError,
    ReadonlyMap<PoolSetName.Type, PoolStats>
  > = usePoolStats()

  const [filter, setFilter] = useState({
    text: "",
    type: FilterOn.StableCoins,
  })

  function onFilterChange(event: object) {
    return setFilter({ ...filter, ...event })
  }

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
    const icon = require(`assets/coins/bBTC.png`).default
    return (
      <TableRow
        hover={true}
        key={poolSetNameStr}
        onClick={() => handleRowClick(poolName)}
      >
        {/* POOL */}
        <StyledTableCell component="th" scope="row">
          <Box display="flex" alignItems="center">
            <Box>
              <img
                src={icon}
                alt={poolSetNameStr}
                style={{ width: "30px", marginRight: "15px" }}
              />
            </Box>
            <Box
              display={"flex"}
              flexDirection={"column"}
              justifyContent={"center"}
            >
              <Box textAlign="left">{poolName}</Box>
              <Box fontWeight={300}>
                {/*keys(ps.reserves).join(" + ")*/}
                {" + "}
              </Box>
            </Box>
          </Box>
        </StyledTableCell>
        {/* Liquidity */}
        <StyledTableCell>
          {O.fold(() => "", printCurrencyUSD)(ps.navUSD)}
        </StyledTableCell>
        {/* Base APY */}
        <StyledTableCell>
          {printPercentage(
            O.getOrElse(() => Percent.iso.wrap(0))(ps.recentDailyAPYPercent)
          )}
        </StyledTableCell>
        {/* Rewards APY */}
        <StyledTableCell>
          {/* {poolRewardAPYs[i]} */}
          {`+4.30% → 10.76% DANA + 1.13% BTC`}
        </StyledTableCell>
        {/* VOLUME */}
        <StyledTableCell>
          {printCurrencyUSD(
            O.getOrElse(() => USD.iso.wrap(0))(ps.recentDailyVolumeUSD.trade)
          )}
        </StyledTableCell>
        {/* APY */}
        <StyledTableCell>
          {printPercentage(
            O.getOrElse(() => Percent.iso.wrap(0))(ps.recentDailyAPYPercent)
          )}
        </StyledTableCell>
      </TableRow>
    )
  }

  function renderTable(
    poolStatsMap: ReadonlyMap<PoolSetName.Type, PoolStats>
  ): JSX.Element {
    return (
      <TableContainer component={Box} className={cx(classes.panel)}>
        <Table aria-label="simple table">
          <TableHead>
            <TableRow>
              {columns.map((column: string, index: number) => {
                return (
                  <StyledTableCell key={index}>
                    {column}
                    &nbsp;
                    <i className="fas fa-sort" />
                  </StyledTableCell>
                )
              })}
            </TableRow>
          </TableHead>
          <TableBody>
            {/* TODO: is `collect` the correct, performant solution? */}
            {ROM.collect(PoolSetName.Ord)(renderTableRow)(poolStatsMap)}
          </TableBody>
          {overview && (
            <TableFooter>
              <TableRow>
                <StyledTableCell colSpan={12}>
                  <Box component="span" onClick={() => history.push("/pools")}>
                    See All Pools
                  </Box>
                </StyledTableCell>
              </TableRow>
            </TableFooter>
          )}
        </Table>
      </TableContainer>
    )
  }

  function renderPoolStats(
    psrd: RemoteData<FetchDecodeError, ReadonlyMap<PoolSetName.Type, PoolStats>>
  ): JSX.Element {
    switch (psrd._tag) {
      case "Success":
        return renderTable(psrd.success)
      case "Pending":
        return <div>loading …</div>
      case "Failure":
        return (
          <details>
            <summary>Error</summary>
            <pre>{JSON.stringify(psrd.failure, null, 2)}</pre>
          </details>
        )
    }
  }

  return (
    <Box>
      {!overview && (
        <Box className={cx(classes.filter)}>
          <SearchInput
            className={cx(classes.filterText)}
            value={filter.text}
            placeholder="Search pool by name, network or type…"
            onChange={(e: any) => {
              onFilterChange({ text: e.target.value })
            }}
          />

          <Box textAlign="center" mt={mobile ? "20px" : "0px"}>
            <Button
              variant="contained"
              onClick={() => {
                onFilterChange({ type: FilterOn.StableCoins })
              }}
              className={cx(classes.filterType, {
                [classes.active]: filter.type === FilterOn.StableCoins,
              })}
            >
              {/* TODO: text-transform: uppercase */}
              STABLECOINS
            </Button>
            <Button
              variant="contained"
              onClick={() => {
                onFilterChange({ type: FilterOn.DigitalAssets })
              }}
              className={cx(classes.filterType, {
                [classes.active]: filter.type === FilterOn.DigitalAssets,
              })}
            >
              {/* TODO: text-transform: uppercase */}
              DIGITAL ASSETS
            </Button>
          </Box>
        </Box>
      )}
      {renderPoolStats(poolStats)}
    </Box>
  )
}

export default PoolsPanel
