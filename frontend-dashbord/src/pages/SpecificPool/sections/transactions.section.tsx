import React from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { TransactionTable } from "components";

const useStyles = makeStyles(({ palette }) => ({
  self: {},
  label: {
    fontFamily: 'Brandon Grotesque',
    fontStyle: 'normal',
    fontWeight: 900,
    fontSize: '18px',
    lineHeight: '110%',
    color: palette.secondary.main,
  }
}));

const TransactionsSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.self)}>
      <Box className={cx(classes.label)}>Transactions</Box>
      <TransactionTable />
    </Box>
  );
};

export default TransactionsSection;
