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

const FILTER_ALL = 0;
const FILTER_NATIVE = 1;
const FILTER_ERC20 = 2;
const FILTER_BEP2 = 3;

const StyledTableCell = withStyles(({ palette }) => ({
  head: {
    fontFamily: "'Museo Sans 300'",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "18px",
    lineHeight: "300%",
    textAlign: "center",
    cursor: "pointer",
    color: palette.secondary.main,
  },
  body: {
    fontFamily: "'Museo Sans 300'",
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
      fontFamily: "'Museo Sans 300'",
      fontStyle: "normal",
      fontWeight: "bold",
      fontSize: "18px",
      lineHeight: "115%",
      color: palette.secondary.main,
    },
  },
}))(TableCell);

const useStyles = makeStyles(({ palette }) => ({
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
    lineHeight: "100%",
    width: "500px",
    padding: "15px 30px",
  },
  filterType: {
    background: palette.secondary.dark,
    width: "150px",
    padding: "15px",
    fontSize: "13px",
    lineHeight: "100%",
    marginLeft: "10px",
  },

  active: {
    background: palette.primary.light,
  },
}));

export interface PoolsPanelProps {
  data: any;
  overView?: boolean;
}

const PoolsPanel: React.FC<PoolsPanelProps> = ({ data, overView = false }) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  const { columns, records } = data;

  const [filter, setFilter] = useState({
    text: "",
    type: 0,
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

          <Box>
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
                  {column.name}
                  &nbsp;
                  <i className="fas fa-sort" />
                </StyledTableCell>
              ))}
            </TableRow>
          </TableHead>
          <TableBody>
            {records.map((row: any, i: any) => (
              <TableRow key={i}>
                <StyledTableCell component="th" scope="row">
                  <Box display="flex">
                    <Box>
                      <img
                        src={row.pool.icon}
                        alt={"coin"}
                        style={{ marginRight: "15px" }}
                      />
                    </Box>
                    <Box
                      display={"flex"}
                      flexDirection={"column"}
                      justifyContent={"center"}
                    >
                      <Box textAlign="left">{row.pool.currency}</Box>
                      <Box fontWeight={300}>{row.pool.description}</Box>
                    </Box>
                  </Box>
                </StyledTableCell>
                <StyledTableCell>{row.baseAPY}</StyledTableCell>
                <StyledTableCell>{row.rewardsAPY}</StyledTableCell>
                <StyledTableCell>{row.volume}</StyledTableCell>
                <StyledTableCell>{row.APY}</StyledTableCell>
              </TableRow>
            ))}
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
