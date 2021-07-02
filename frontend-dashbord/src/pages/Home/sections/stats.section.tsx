import React from "react";
import { Box, Grid, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette }) => ({
  self: {},
  title: {
    fontFamily: "Brandon Grotesque Bold",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "30px",
    lineHeight: "110%",
    marginBottom: "20px",
    color: palette.secondary.main,
  },
  body: {
    background: palette.background.paper,
    padding: "30px 50px 30px 50px",
    borderRadius: "10px",

    "& p": {
      fontFamily: "'Museo Sans 300'",
      fontStyle: "normal",
      fontWeight: "bold",
      fontSize: "18px",
      lineHeight: "15px",
      whiteSpace: "pre-line",
      color: palette.secondary.main,
    },
  },
  body_ex: {
    background: `${
      palette.type === "light" ? palette.common.white : palette.common.black
    }`,
    borderRadius: "25px",
    fontFamily: "Brandon Grotesque Bold",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "30px",
    lineHeight: "43px",
    textAlign: "center",
    padding: "10px",
    color: palette.secondary.main,
  },
}));

const StatsSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.self)}>
      <Grid container spacing={3}>
        <Grid item xs={12} sm={6}>
          <Box className={cx(classes.title)}>exDANA Stats</Box>
          <Box className={cx(classes.body)}>
            <Box component="p">
              {`exDANA holder/LP ratio (based on fees): 24.52
              \n
              Having locked $1 in DANA for 4 years is equal to having provided $24.52 as an LP
              \n
              exDANA holder APY: 21.37% (4 weeks average: 18.01%)
              Yearly fee earnings per 1 exDANA: $0.34
              \n
              My exDANA balance: 0 Stake DANA
              \n
              Averaged daily earnings: $198,244.20
              \n
              Weekly earnings: $1,387,709.41
              \n
              Weekly volume (including deposits/withdrawals): $6,938,547,054.70
              \n
              Next Distribution: Mon, 28 Jun 2021 23:20:50 GMT`}
            </Box>
          </Box>
        </Grid>
        <Grid item xs={12} sm={6}>
          <Box className={cx(classes.title)}>
            Total Pool Deposits and Daily Volume
          </Box>
          <Box className={cx(classes.body)}>
            <Box component="p">
              {`DEPOSITS:
              $9,006,029,010.68 (includes factory pools)
              \n
              DAILY VOLUME:
              $147,471,544
              \n
              Factory Daily Volume:
              $3,065,174
              `}
            </Box>
          </Box>

          <Box mt="30px" />

          <Box className={cx(classes.title)}>exDANA 2pool LP Claim:</Box>
          <Box className={cx(classes.body_ex)}>Claim 9999.122</Box>
        </Grid>
      </Grid>
    </Box>
  );
};

export default StatsSection;
