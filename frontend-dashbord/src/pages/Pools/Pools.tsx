import React from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { pools } from "data";
import PoolsPanel from "components/Table";
import { useIsDarkMode } from "state/user/hooks";
import cx from "classnames";

const useStyles = makeStyles(({ palette }) => ({
  label: {
    color: palette.secondary.main,
    fontFamily: "'Brandon Grotesque'",
    fontStyle: "normal",
    fontWeight: "normal",
    fontSize: "30px",
    lineHeight: "110%",
    margin: "30px 0",
  },
  statsPanel: {
    background:
      palette.type === "light"
        ? "#F6F6F6"
        : "linear-gradient(180deg, #131B59 0%, #2F3DA0 100%)",
    borderRadius: "10px",
    border: palette.type === "light" ? "1px solid #C4C4C4" : "unset",
    padding: "50px 70px",

    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: "18px",
    lineHeight: "115%",
    whiteSpace: "pre-line",

    color: palette.text.secondary,

    "& span": {
      fontWeight: 400,
    },
  },
}));

const Pools: React.FC = () => {
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const dark = useIsDarkMode();
  const classes = useStyles({ dark, mobile });

  return (
    <Box>
      <Box className={cx(classes.label)}>Dana Pools</Box>
      <PoolsPanel data={pools}></PoolsPanel>

      <Box className={cx(classes.label)}>
        Total pool deposits and daily volume
      </Box>
      <Box className={cx(classes.statsPanel)}>
        Deposit: <span>$24,000.000,000.56 (includes factory pools)</span>{'\n\n'}
        Daily Volume: <span>$111,000,000</span>{'\n\n'}
        Factory Daily Volume: <span>$8,999,777</span>
      </Box>
    </Box>
  );
};

export default Pools;
