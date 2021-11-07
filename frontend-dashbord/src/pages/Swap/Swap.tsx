import React, { useState } from "react";
import {
  Box,
  Collapse,
  Fade,
  Grid,
  Mark,
  useMediaQuery,
} from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { TokenBox } from "components/Box";
import { Slider } from "components";
import { Radio, SwapButton } from "components/Button";
import { options } from "data";

import ICO_Info_dark from "assets/svg/info_dark.svg";
import { Currency } from "state/wallet/actions";
import { useWallet } from "state/wallet/hooks";

const useStyles = makeStyles(({ palette }) => ({
  self: {
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
  },

  panel: {
    width: "500px",
    background:
      palette.type === "light"
        ? palette.common.white
        : "linear-gradient(180deg, rgba(0, 0, 0, 0) 0%, #0C1347 100%)",
    boxShadow: "inset 0px 4px 4px rgba(0, 0, 0, 0.25)",
    backdropFilter: "blur(4px)",
    borderRadius: "10px",
  },

  panelHeader: {
    position: "relative",
    width: "100%",
    background:
      palette.type === "light"
        ? "linear-gradient(180deg, #FFFFFF 42.71%, #E9E9E9 100%)"
        : "linear-gradient(180deg, rgba(19, 27, 89, 0) 0%, #131B59 100%)",
    borderRadius: "5px",
    boxShadow: "0px 4px 4px rgba(0, 0, 0, 0.25)",

    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "15px",
    lineHeight: "100%",
    color: palette.secondary.main,

    padding: "20px",
    textAlign: "center",
  },

  leftBorder: {
    position: "absolute",
    left: 0,
    top: 0,
    bottom: 0,
    height: "100%",
    background: palette.info.dark,
    borderRadius: "5px",
    fontFamily: "auto",
  },

  panelBody: {
    padding: "20px 10px",
  },

  box: {
    background: palette.type === "light" ? palette.common.white : "#0C1347",
    borderRadius: "5px",
    boxShadow:
      palette.type === "light"
        ? "2px 2px 10px rgba(0, 0, 0, 0.1)"
        : "0px 4px 4px rgba(0, 0, 0, 0.25)",
  },

  slider: {
    display: "flex",
    alignItems: "center",
    margin: "10px 0px 10px 10px",
  },

  info: {
    display: "flex",
    alignItems: "center",
    justifyContent: "space-between",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "15px",
    lineGeight: "100%",
    color: palette.secondary.main,
  },

  optionTitle: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "16px",
    lineHeight: "24px",
    color: palette.type === "light" ? palette.text.primary : palette.text.hint,
    display: "flex",
    alignItems: "center",
    cursor: "pointer",
  },

  submit: {
    background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
    borderRadius: "20px",
    width: "250px",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: "bold",
    fontSize: "24px",
    lineHeight: "100%",
    textAlign: "center",
    color: palette.common.white,
    padding: "15px",
    cursor: "pointer",

    "&:hover": {
      background: "linear-gradient(-90deg, #5F72FF 0%, #73D6F1 100%)",
    },
  },
}));

const Swap: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  const {
    wallet: { balances: TokenList },
  } = useWallet();

  const [fromAmount, setFromAmount] = useState(0);
  const [toAmount, setToAmount] = useState(0);

  const [fromToken, setFromToken] = useState<Currency | undefined>(
    TokenList.find((token: Currency) => token.unit === "dana")
  );
  const [toToken, setToToken] = useState<Currency | undefined>(
    TokenList.find((token: Currency) => token.unit === "ada")
  );

  const [isOptionOpen, setIsOptionOpen] = useState(false);

  const marks: Mark[] = [
    {
      value: 0,
      label: "0%",
    },
    {
      value: fromToken?.quantity ?? 0,
      label: "100%",
    },
  ];

  const onToggleOptions = () => {
    setIsOptionOpen((prev) => !prev);
  };

  const onAmountChange = (e: any, newValue: any) => {
    setFromAmount(newValue);
  };

  const onSwapButtonClick = (e: any) => {
    setFromAmount(toAmount);
    setToAmount(fromAmount);
    setFromToken(toToken);
    setToToken(fromToken);
  };

  return (
    <Fade in={true}>
      <Box className={cx(classes.self)}>
        <Box className={cx(classes.panel)}>
          <Box className={cx(classes.panelHeader)}>
            <Box className={cx(classes.leftBorder)}>&nbsp;&nbsp;</Box>
            <Box>Swap using all Ardana pools</Box>
          </Box>
          <Box className={cx(classes.panelBody)}>
            <TokenBox
              label="SEND"
              token={fromToken}
              amount={fromAmount}
              onAmountChange={(amount: number) => {
                setFromAmount(Number(amount));
              }}
              handleTokenSelect={(token: Currency) => {
                setFromToken(token);
                if (token === toToken) {
                  setToToken(fromToken);
                }
              }}
              className={cx(classes.box)}
              style={{ padding: 10 }}
            />

            <Box className={cx(classes.slider)}>
              <Slider
                min={0}
                max={fromToken?.quantity ?? 0}
                defaultValue={0}
                value={typeof fromAmount === "number" ? fromAmount : 0}
                onChange={onAmountChange}
                step={1}
                marks={marks}
              />
              <SwapButton
                style={{ margin: "0 40px" }}
                onButtonClick={onSwapButtonClick}
              />
            </Box>

            <TokenBox
              label="RECEIVE"
              token={toToken}
              amount={toAmount}
              onAmountChange={(amount: number) => {
                setToAmount(Number(amount));
              }}
              handleTokenSelect={(token: Currency) => {
                setToToken(token);
                if (token === fromToken) {
                  setFromToken(toToken);
                }
              }}
              className={cx(classes.box)}
              style={{ padding: 10 }}
            />

            <Box mt={"50px"}></Box>

            <Box className={cx(classes.box)} style={{ padding: "30px 20px" }}>
              <Box className={cx(classes.info)}>
                <Box>Rate DANA/ANA (including fees):</Box>
                <Box display="flex" alignItems="center" lineHeight="1">
                  1.005&nbsp;
                  <img src={ICO_Info_dark} width="13px" alt="info" />
                </Box>
              </Box>
              <Box className={cx(classes.info)}>
                <Box>Slip:</Box>
                <Box display="flex" alignItems="center" lineHeight="1">
                  1%&nbsp;
                  <img src={ICO_Info_dark} width="13px" alt="info" />
                </Box>
              </Box>
              <Box className={cx(classes.info)}>
                <Box>Trade routed through:</Box>
                <Box display="flex" alignItems="center" lineHeight="1">
                  exDANA&nbsp;
                  <img src={ICO_Info_dark} width="13px" alt="info" />
                </Box>
              </Box>
            </Box>

            <Box mt={"50px"}></Box>

            <Box padding="10px">
              <Box
                className={cx(classes.optionTitle)}
                onClick={onToggleOptions}
              >
                <Box mr={"7px"}>Advanced Options</Box>
                <i
                  className={`fa fa-chevron-${!isOptionOpen ? "up" : "down"}`}
                  aria-hidden="true"
                ></i>
              </Box>
              <Collapse in={isOptionOpen}>
                <Grid
                  container
                  spacing={mobile ? 1 : 2}
                  style={{ marginTop: "10px" }}
                >
                  {options.map((option, i) => (
                    <Grid container item xs={4} key={`options${i}`}>
                      <Radio option={option} value={option.data[0].value} />
                    </Grid>
                  ))}
                </Grid>
              </Collapse>
            </Box>

            <Box mt="50px"></Box>

            <Box display="flex" justifyContent="center">
              <Box className={cx(classes.submit)}>SWAP</Box>
            </Box>
          </Box>
        </Box>
      </Box>
    </Fade>
  );
};

export default Swap;
