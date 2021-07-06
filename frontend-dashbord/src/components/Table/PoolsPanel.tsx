import React from "react";
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
      fontStyle: 'normal',
      fontWeight: 'bold',
      fontSize: '18px',
      lineHeight: '115%',
      color: palette.secondary.main,
    }
  },
}))(TableCell);

const useStyles = makeStyles(({ palette }) => ({
  panel: {
    background: palette.secondary.light,
    borderRadius: "10px",
    padding: "12px",
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
  return (
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
        <TableFooter>
          <TableRow>
            <StyledTableCell colSpan={12}>
              <Link href="/pools">See All Pools</Link>
            </StyledTableCell>
          </TableRow>
        </TableFooter>
      </Table>
    </TableContainer>
  );
};

export default PoolsPanel;
