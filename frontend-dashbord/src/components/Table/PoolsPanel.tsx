import React, { useState } from "react";
import {
  Box,
  useMediaQuery,
  Link,
  TableContainer,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
  TableFooter,
} from "@material-ui/core";
import { makeStyles, useTheme, withStyles } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";
import { SearchInput } from "components/Input";
import { Button } from "components/Button";
import { usePoolStats } from "state/home/hooks";
import { keys, values } from "lodash";

const FILTER_ALL = 0;
const FILTER_NATIVE = 1;
const FILTER_ERC20 = 2;
const FILTER_BEP2 = 3;

const StyledTableCell = withStyles(({ palette }) => ({
  head: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "18px",
    lineHeight: "300%",
    textAlign: "center",
    cursor: "pointer",
    color: palette.secondary.main,
  },
  body: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: "bold",
    fontSize: "18px",
    lineHeight: "115%",
    textAlign: "center",
    color: palette.secondary.main,
  },
  footer: {
    width: "100%",
    height: 30,
    textAlign: "center",

    "& a": {
      fontFamily: "Museo Sans",
      fontStyle: "normal",
      fontWeight: "bold",
      fontSize: "18px",
      lineHeight: "115%",
      color: palette.secondary.main,
    },
  },
}))(TableCell);

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  panel: {
    background: palette.secondary.light,
    borderRadius: "10px",
    padding: "12px",
  },
  filter: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    margin: "20px 0",

    [breakpoints.down("xs")]: {
      flexDirection: "column",
      "& > div": {
        width: "100%",
      },
    },
  },

  filterText: {
    background:
      palette.type === "light"
        ? "#E7E7E7"
        : "linear-gradient(90deg, #3142A3 0%, #243183 100%)",
    boxShadow:
      palette.type === "light"
        ? "unset"
        : "2px 2px 10px rgba(0, 0, 0, 0.1), inset 2px 2px 10px rgba(0, 0, 0, 0.1)",
    fontSize: "13px",
    fontWeight: 600,
    lineHeight: "100%",
    width: "500px",
    padding: "15px 30px",
    borderRadius: "20px",
    color: palette.secondary.main,

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
    fontSize: "13px",
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
}));

export interface PoolsPanelProps {
  overView?: boolean;
}

const PoolsPanel: React.FC<PoolsPanelProps> = ({ overView = false }) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const columns = ["POOL", "Base APY", "Rewards APY", "VOLUME", "APY"];

  const poolStats = usePoolStats();
  const poolNames = keys(poolStats);
  const poolInfos = values(poolStats);
  const poolReserves = poolInfos.map((val: any) => {
    let reserves = keys(val.reserves);
    return reserves.join(" + ");
  });
  const poolRewardAPYs = poolInfos.map((val: any) => {
    const { reserves, totalAPYPercent, recentAnnualAPYPercent } = val;
    const reserveNames = keys(reserves);
    const reserveVals = values(reserves);
    const formattedReserves: string[] = reserveNames.map(
      (reserveName: string, i: number) => {
        return reserveVals[i] + "% " + reserveName;
      }
    );
    const changedPercent = totalAPYPercent - recentAnnualAPYPercent;

    return (
      (changedPercent < 0 ? "" : "+") +
      changedPercent +
      "%" +
      " -> " +
      formattedReserves.join(" + ")
    );
  });

  const [filter, setFilter] = useState({
    text: "",
    type: FILTER_ALL,
  });

  const onFilterChange = (event: any) => {
    setFilter({ ...filter, ...event });
  };

  return (
    <Box>
      {!overView && (
        <Box className={cx(classes.filter)}>
          <SearchInput
            className={cx(classes.filterText)}
            value={filter.text}
            placeholder="Search pool by name, network or type..."
            onChange={(e: any) => {
              onFilterChange({ text: e.target.value });
            }}
          />

          <Box textAlign="center" mt={mobile ? "20px" : "0px"}>
            <Button
              variant="contained"
              onClick={() => {
                onFilterChange({ type: FILTER_ALL });
              }}
              className={cx(classes.filterType, {
                [classes.active]: filter.type === FILTER_ALL,
              })}
            >
              All
            </Button>
            <Button
              variant="contained"
              onClick={() => {
                onFilterChange({ type: FILTER_NATIVE });
              }}
              className={cx(classes.filterType, {
                [classes.active]: filter.type === FILTER_NATIVE,
              })}
            >
              NATIVE
            </Button>
            <Button
              variant="contained"
              onClick={() => {
                onFilterChange({ type: FILTER_ERC20 });
              }}
              className={cx(classes.filterType, {
                [classes.active]: filter.type === FILTER_ERC20,
              })}
            >
              ERC20
            </Button>
            <Button
              variant="contained"
              onClick={() => {
                onFilterChange({ type: FILTER_BEP2 });
              }}
              className={cx(classes.filterType, {
                [classes.active]: filter.type === FILTER_BEP2,
              })}
            >
              BEP2
            </Button>
          </Box>
        </Box>
      )}
      <TableContainer component={Box} className={cx(classes.panel)}>
        <Table aria-label="simple table">
          <TableHead>
            <TableRow>
              {columns.map((column: any, i: any) => (
                <StyledTableCell key={i}>
                  {column}
                  &nbsp;
                  <i className="fas fa-sort" />
                </StyledTableCell>
              ))}
            </TableRow>
          </TableHead>
          <TableBody>
            {poolNames.map((poolName: any, i: any) => {
              const icon = require(`assets/coins/${poolName}.png`).default;
              // const icon = require(`assets/coins/bBTC.png`).default;
              return (
                <TableRow key={i}>
                  <StyledTableCell component="th" scope="row">
                    <Box display="flex">
                      <Box>
                        <img
                          src={icon}
                          alt={poolName}
                          style={{ marginRight: "15px" }}
                        />
                      </Box>
                      <Box
                        display={"flex"}
                        flexDirection={"column"}
                        justifyContent={"center"}
                      >
                        <Box textAlign="left">{poolName}</Box>
                        <Box fontWeight={300}>{poolReserves[i]}</Box>
                      </Box>
                    </Box>
                  </StyledTableCell>
                  <StyledTableCell>
                    {poolInfos[i].recentDailyAPYPercent}%
                  </StyledTableCell>
                  <StyledTableCell>{poolRewardAPYs[i]}</StyledTableCell>
                  <StyledTableCell>
                    ${poolInfos[i].recentDailyVolumeUSD}
                  </StyledTableCell>
                  <StyledTableCell>
                    {poolInfos[i].totalAPYPercent}%
                  </StyledTableCell>
                </TableRow>
              );
            })}
          </TableBody>
          {overView && (
            <TableFooter>
              <TableRow>
                <StyledTableCell colSpan={12}>
                  <Link href="/pools">See All Pools</Link>
                </StyledTableCell>
              </TableRow>
            </TableFooter>
          )}
        </Table>
      </TableContainer>
    </Box>
  );
};

export default PoolsPanel;
