import React, { Fragment } from "react";
import { Box, Grid, useMediaQuery, Link } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";
import Record from "./Record";
import { Button } from "components/Button";

const useStyles = makeStyles(({ palette }) => ({
  panel: {
    backgroundColor: palette.secondary.main,
    borderRadius: "5px",
    padding: "4% 50px 14px 6%",
  },
  panelMobile: {
    padding: "8px 14px",
  },
  filterItem: {
    fontSize: "15px",
    borderRadius: "25px",
    padding: "6px 18px",
    margin: "0px 5px",
    color: palette.common.black,
    cursor: "pointer",
    transition: "all .3s ease-in",
    "&:hover": {
      backgroundColor: "#bcbcbc",
      color: palette.text.hint,
      transition: "all .2s ease-in",
    },
  },
  activeItem: {
    backgroundColor: palette.common.white,
  },
}));

const filterTypes = [
  "All",
  "USD",
  "BTC",
  "ETH",
  "Crypto",
  "Others",
  "My Dashboard",
];

export interface PoolsPanelProps {
  data?: any;
  filterType?: string;
  overView?: boolean;
}

const PoolsPanel: React.FC<PoolsPanelProps> = ({
  data,
  filterType = "All",
  overView = false,
}) => {
  const { palette, breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  return (
    <Box className={cx(classes.panel, mobile && classes.panelMobile)}>
      {!mobile && (
        <Fragment>
          <Box
            display="flex"
            justifyContent="space-between"
            width="100%"
            alignItems={"flex-start"}
          >
            <Box fontSize={"36px"}>Ardada Pools</Box>
            {!overView && (
              <Box
                display={"flex"}
                bgcolor={"#E5E5E5"}
                p={"4px 0px"}
                borderRadius={"50px"}
              >
                {filterTypes.map((type, i) => (
                  <Box
                    key={i}
                    className={cx(
                      classes.filterItem,
                      filterType === type && classes.activeItem
                    )}
                  >
                    {type}
                  </Box>
                ))}
              </Box>
            )}
          </Box>
          <Box>
            <Grid
              container
              style={{
                fontSize: "18px",
                color: palette.text.primary,
                marginTop: "55px",
                padding: "0 26px",
              }}
              spacing={3}
            >
              {data.columns.map((column: any, i: any) => (
                <Grid container item xs={column.col} key={i}>
                  {column.name}
                </Grid>
              ))}
            </Grid>
          </Box>
        </Fragment>
      )}
      {data.records.map((record: any, i: any) => (
        <Record data={record} key={i}></Record>
      ))}

      {overView && (
        <Box textAlign={"right"}>
          <Link href="/pools">
            <Button variant="contained">See All Pools</Button>
          </Link>
        </Box>
      )}
    </Box>
  );
};

export default PoolsPanel;
