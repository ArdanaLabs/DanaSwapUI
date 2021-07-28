import React from "react";
import { Box, Fade, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import PoolsPanel from "components/Table";
import { useIsDarkMode } from "state/user/hooks";
import cx from "classnames";
import { useTotalStats } from "state/home/hooks";

const useStyles = makeStyles(({ palette }) => ({
  label: {
    color: palette.secondary.main,
    fontFamily: "'Brandon Grotesque'",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "28px",
    lineHeight: "110%",
    margin: "30px 0",
  },
  statsPanel: {
    background:
      palette.type === "light"
        ? "#F6F6F6"
        : palette.background.paper,
    borderRadius: "10px",
    border: palette.type === "light" ? "1px solid #C4C4C4" : "unset",
    padding: "50px 70px",

    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "16px",
    lineHeight: "115%",
    whiteSpace: "pre-line",

    color: palette.text.secondary,

    "& span": {
      fontWeight: 300,
    },
  },
}));

const Pools: React.FC = () => {
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const dark = useIsDarkMode();
  const classes = useStyles({ dark, mobile });

  const totalStats = useTotalStats();
  const { totalDepositsAllPoolsUSD, totalDailyVolumeUSD } = totalStats;

  return (
    <Fade in={true}>
      <Box>
        <Box className={cx(classes.label)}>Dana Pools</Box>
        <PoolsPanel />

        <Box className={cx(classes.label)}>
          Total pool deposits and daily volume
        </Box>
        <Box className={cx(classes.statsPanel)}>
          Deposit:{" "}
          <span>
          ${totalDepositsAllPoolsUSD ? totalDepositsAllPoolsUSD.toLocaleString() : 0} (includes factory pools)
          </span>
          <br />
          <br />
          Daily Volume: <span>${totalDailyVolumeUSD ? totalDailyVolumeUSD.toLocaleString() : 0}</span>
          <br />
          <br />
          Factory Daily Volume: <span>$8,999,777</span>
        </Box>
      </Box>
    </Fade>
  );
};

export default Pools;
