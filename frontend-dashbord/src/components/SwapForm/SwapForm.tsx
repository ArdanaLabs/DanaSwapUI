import React, { useState } from "react";
import { Box, Grid, MenuItem, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";
import Input from "components/Input";
import SwapButton from "components/Button/SwapButton";
import CoinSelect from "components/Select";

import IconCoin1 from "assets/coin1.png";
import IconCoin2 from "assets/coin2.png";
import IconCoin3 from "assets/coin3.png";

const coinList = [
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

const useStyles = makeStyles(({ palette }) => ({
  chooseCoin: {
    height: "28px",
    width: "148px",
    color: palette.text.secondary,
    fontSize: "12px",
    textAlign: "center",
    backgroundColor: palette.common.white,
    borderRadius: "100px",
    fontWeight: "bold",
    lineHeight: "28px",
    marginTop: "24px",
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
  },
  panelMobile: {
    display: "flex",
    flexDirection: "column",
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

  const [amount1, setAmount1] = useState("");
  const [amount2, setAmount2] = useState("");

  const [coin1, setCoin1] = useState("");
  const [coin2, setCoin2] = useState("");

  const onSwapOptions = (): any => {
    setAmount1(amount2);
    setAmount2(amount1);

    setCoin1(coin2);
    setCoin2(coin1);
  };

  return (
    <Grid container spacing={mobile ? 1 : 2} style={{ position: "relative" }}>
      <Grid container item xs={6}>
        <Box
          position={"sticky"}
          width={"100%"}
          bgcolor={palette.secondary.main}
          pt={mobile ? "15px" : "37px"}
          pb={mobile ? "28px" : "37px"}
          pl={mobile ? "14px" : "37px"}
          pr={mobile ? "14px" : "37px"}
          borderRadius={"5px"}
          className={cx(classes.panel, {
            [classes.panelMobile]: mobile,
          })}
        >
          <Box
            className={cx(classes.coinBox, {
              [classes.coinBoxMobile]: mobile,
            })}
          >
            <Box
              width={mobile ? "81px" : "138px"}
              height={mobile ? "81px" : "138px"}
              bgcolor={palette.common.white}
              borderRadius={"100%"}
            ></Box>
            <CoinSelect
              value={coin1}
              onChange={(e: any): any => setCoin1(e.target.value)}
              displayEmpty={true}
              inputProps={{ "aria-label": "Without label" }}
              className={cx(classes.chooseCoin, {
                [classes.chooseCoinMobile]: mobile,
              })}
            >
              <MenuItem value="" disabled>
                Choose Coin
              </MenuItem>
              {coinList.map((item, index) => (
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
              value={amount1}
              onChange={(e: any): any => setAmount1(e.target.value)}
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
        <SwapButton></SwapButton>
      </Box>
      <Grid container item xs={6}>
        <Box
          position={"sticky"}
          width={"100%"}
          bgcolor={palette.secondary.main}
          pt={mobile ? "15px" : "37px"}
          pb={mobile ? "28px" : "37px"}
          pl={mobile ? "14px" : "37px"}
          pr={mobile ? "14px" : "37px"}
          borderRadius={"5px"}
          className={cx(classes.panel, {
            [classes.panelMobile]: mobile,
          })}
        >
          <Box
            className={cx(classes.coinBox, {
              [classes.coinBoxMobile]: mobile,
            })}
          >
            <Box
              width={mobile ? "81px" : "138px"}
              height={mobile ? "81px" : "138px"}
              bgcolor={palette.common.white}
              borderRadius={"100%"}
            ></Box>
            <CoinSelect
              value={coin2}
              onChange={(e: any): any => setCoin2(e.target.value)}
              displayEmpty={true}
              inputProps={{ "aria-label": "Without label" }}
              className={cx(classes.chooseCoin, {
                [classes.chooseCoinMobile]: mobile,
              })}
            >
              <MenuItem value="" disabled>
                Choose Coin
              </MenuItem>
              {coinList.map((item, index) => (
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
              value={amount2}
              onChange={(e: any): any => setAmount2(e.target.value)}
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
