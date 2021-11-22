import React from "react";
import cx from "classnames";
import { Box } from "@material-ui/core";
import { makeStyles } from "@material-ui/core/styles";

const useStyles = makeStyles(({ palette }) => ({
  root: {
    margin: "0 10px",
    cursor: "pointer",
    display: "flex",
    alignItems: "center",
  },
  label: {
    fontSize: "14px",
    fontWeight: 700,
    fontFamily: "Museo Sans",
    color: palette.primary.main,
    flexGrow: 5,
    textAlign: "center",
  },
  icon: {
    marginLeft: "10px",
    color: palette.primary.main,
  },
}));

const ConnectWallet: React.FC = () => {
  const classes = useStyles();

  const handleConnectWallet = () => {
    console.log("connect wallet button clicked!");
  };

  return (
    <Box className={cx(classes.root)} onClick={handleConnectWallet}>
      <Box className={cx(classes.label)}>CONNECT A WALLET</Box>
      <i className={cx(classes.icon, "fas fa-arrow-right")} />
    </Box>
  );
};

export default ConnectWallet;
