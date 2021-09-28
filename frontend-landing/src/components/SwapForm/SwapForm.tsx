import React, { useState } from "react";
import { Box, Grid, MenuItem, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";
import { Input } from "components/Input";
import SwapButton from "components/Button/SwapButton";
import CoinSelect from "components/Select";

import IconCoin1 from "assets/coin1.png";
import IconCoin2 from "assets/coin2.png";
import IconCoin3 from "assets/coin3.png";

const CoinList = [
  {
    icon: IconCoin1,
    label: "Bitcoin (BTC)",
  },
  {
    icon: IconCoin2,
    label: "Cardano (ADA)",
  },
  {
    icon: IconCoin3,
    label: "Ethereum (ETH)",
  },
];

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  chooseCoin: {
    height: "28px",
    width: "148px",
    color: palette.text.secondary,
    fontSize: "12px",
    textAlign: "center",
    background: palette.background.default,
    borderRadius: "100px",
    fontWeight: "bold",
    lineHeight: "28px",
    marginTop: "24px",
    transition: "background .3s ease-in",
  },
  chooseCoinMobile: {
    height: "25.5px",
    lineHeight: "25.5px",
    width: "136px",
    marginTop: "15px",
  },
  coinBox: {
    width: "150px",
    display: "flex",
    flexDirection: "column",
    alignItems: "center",
    order: 1,
  },
  coinBoxMobile: {
    order: 0,
  },
  panel: {
    display: "flex",
    float: "right",
    alignItems: "center",
    position: "sticky",
    width: "100%",
    background: palette.secondary.main,
    padding: "37px",
    borderRadius: "5px",
    transition: "background .3s ease-in",

    [breakpoints.down("xs")]: {
      flexDirection: "column",
      padding: "14px",
    },
  },
  swapInput: {
    height: "41px",
    marginTop: "20px",
    padding: "7px 17px",
    fontSize: "18px",
    width: "100%",
    maxWidth: "250px",
  },
  swapInputMobile: {},
  menuItem: {
    "& img": {
      width: "20px",
      height: "20px",
    },
    "& span": {
      padding: "0 10px",
    },
  },
}));

export interface SwapFormProps {}

const SwapForm: React.FC<SwapFormProps> = () => {
  const { palette, breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const [fromAmount, setFromAmount] = useState("");
  const [toAmount, setToAmount] = useState("");

  const [fromCoin, setFromCoin] = useState("");
  const [toCoin, setToCoin] = useState("");

  const onSwapOptions = (): any => {
    setFromAmount(toAmount);
    setToAmount(fromAmount);

    setFromCoin(toCoin);
    setToCoin(fromCoin);
  };

  const getIconByName = (name: String): any =>
    CoinList.find((coin) => coin.label === name)!.icon;

  return (
    <Grid container spacing={mobile ? 1 : 2} style={{ position: "relative" }}>
      <Grid container item xs={6}>
        <Box className={cx(classes.panel)}>
          <Box
            className={cx(classes.coinBox, {
              [classes.coinBoxMobile]: mobile,
            })}
          >
            <Box
              width={mobile ? "81px" : "138px"}
              height={mobile ? "81px" : "138px"}
              bgcolor={palette.background.default}
              borderRadius={"100%"}
              display="flex"
              justifyContent="center"
              alignItems="center"
            >
              {fromCoin && (
                <img
                  src={getIconByName(fromCoin)}
                  width="100%"
                  alt="From Coin Icon"
                />
              )}
            </Box>
            <CoinSelect
              value={fromCoin}
              onChange={(e: any): any => setFromCoin(e.target.value)}
              displayEmpty={true}
              inputProps={{ "aria-label": "Without label" }}
              className={cx(classes.chooseCoin, {
                [classes.chooseCoinMobile]: mobile,
              })}
            >
              <MenuItem value="" disabled>
                Choose Coin
              </MenuItem>
              {CoinList.map((item, index) => (
                <MenuItem
                  key={index}
                  value={item.label}
                  className={cx(classes.menuItem)}
                >
                  <img src={item.icon} alt={item.label} />
                  <span>{item.label}</span>
                </MenuItem>
              ))}
            </CoinSelect>
          </Box>
          <Box
            width={"100%"}
            pr={mobile ? "0px" : "20px"}
            mt={mobile ? "20px" : 0}
            textAlign={mobile ? "center" : "auto"}
          >
            <Box
              fontSize={mobile ? "13px" : "18px"}
              color={palette.text.primary}
            >
              Swap Form
            </Box>
            <Box
              fontSize={mobile ? "11px" : "14px"}
              color={palette.text.secondary}
            >
              Please enter your desired amount
            </Box>
            <Input
              className={cx(classes.swapInput, {
                [classes.swapInputMobile]: mobile,
              })}
              value={fromAmount}
              onChange={(e: any): any => setFromAmount(e.target.value)}
              placeholder={"0.00"}
              type={"number"}
              step={"0.01"}
            ></Input>
          </Box>
        </Box>
      </Grid>
      <Box
        position={"absolute"}
        top={"calc(50% - 23px)"}
        left={"calc(50% - 23px)"}
        zIndex={"1"}
        onClick={onSwapOptions}
      >
        {/* <SwapButton></SwapButton> */}
      </Box>
      <Grid container item xs={6}>
        <Box className={cx(classes.panel)}>
          <Box
            className={cx(classes.coinBox, {
              [classes.coinBoxMobile]: mobile,
            })}
          >
            <Box
              width={mobile ? "81px" : "138px"}
              height={mobile ? "81px" : "138px"}
              bgcolor={palette.background.default}
              borderRadius={"100%"}
              display="flex"
              justifyContent="center"
              alignItems="center"
            >
              {toCoin && (
                <img
                  src={getIconByName(toCoin)}
                  width="100%"
                  alt="To Coin Icon"
                />
              )}
            </Box>
            <CoinSelect
              value={toCoin}
              onChange={(e: any): any => setToCoin(e.target.value)}
              displayEmpty={true}
              inputProps={{ "aria-label": "Without label" }}
              className={cx(classes.chooseCoin, {
                [classes.chooseCoinMobile]: mobile,
              })}
            >
              <MenuItem value="" disabled>
                Choose Coin
              </MenuItem>
              {CoinList.map((item, index) => (
                <MenuItem
                  key={index}
                  value={item.label}
                  className={cx(classes.menuItem)}
                >
                  <img src={item.icon} alt={item.label} />
                  <span>{item.label}</span>
                </MenuItem>
              ))}
            </CoinSelect>
          </Box>
          <Box
            width={"100%"}
            pr={mobile ? "0px" : "20px"}
            mt={mobile ? "20px" : 0}
            textAlign={mobile ? "center" : "auto"}
          >
            <Box
              fontSize={mobile ? "13px" : "18px"}
              color={palette.text.primary}
            >
              Swap To
            </Box>
            <Box
              fontSize={mobile ? "11px" : "14px"}
              color={palette.text.secondary}
            >
              Please view the swapped amount here:
            </Box>
            <Input
              className={cx(classes.swapInput, {
                [classes.swapInputMobile]: mobile,
              })}
              value={toAmount}
              onChange={(e: any): any => setToAmount(e.target.value)}
              placeholder={"0.00"}
              type={"number"}
              step={"0.01"}
            ></Input>
          </Box>
        </Box>
      </Grid>
    </Grid>
  );
};

export default SwapForm;
