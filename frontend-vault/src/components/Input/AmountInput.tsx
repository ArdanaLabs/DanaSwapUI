import React, { useState, useEffect } from "react"
import cx from "classnames"

import { ReactComponent as SwapIcon } from "assets/image/svgs/swap.svg"
import { Box, Theme, Typography, useTheme } from "@mui/material"
import { makeStyles } from "@mui/styles"
import { FontFamilies } from "theme"

const useStyles = makeStyles((theme: Theme) => ({
  root: {},
  input: {
    border: `2px solid ${theme.palette.info.dark}`,
    borderRadius: "10px",
    background: `${theme.palette.background.default}55`,
    padding: "10px 20px",
    position: "relative",

    [`& input`]: {
      color: theme.palette.primary.main,
      border: "none",
      outline: "none",
      background: "transparent",
      fontFamily: FontFamilies.Brandon,
      fontStyle: "normal",
      fontWeight: 700,
      fontSize: 45,
      width: "100%",

      [theme.breakpoints.down("sm")]: {
        fontSize: 30,
      },

      [`&::-webkit-outer-spin-button, &::-webkit-inner-spin-button`]: {
        [`-webkit-appearance`]: "none",
        margin: 0,
      },
    },

    [`& > .swap`]: {
      position: "absolute",
      top: "50%",
      right: "10px",
      transform: "translate(0%, -50%)",
      cursor: "pointer",

      [`& path`]: {
        fill: theme.palette.primary.main,
      },
    },
  },
}))

export interface AmountInputProps {
  token: string
  inputChange: (newAmount: number) => void
}

const AmountInput: React.FC<AmountInputProps> = ({ token, inputChange }) => {
  const theme = useTheme()
  const classes = useStyles(theme)

  const [depositCurrency, setDepositCurrency] = useState<string>(token)
  const [tokenInputAmount, setTokenInputAmount] = useState<number>(0)
  const [usdInputAmount, setUsdInputAmount] = useState<number>(0)

  const usdRates = 4304.23
  const usdBalance = 34.4
  const tokenBalance = 0.0235

  const handleInputChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const value = Number(event.target.value)
    if (depositCurrency === "USD") {
      setUsdInputAmount(value)
      setTokenInputAmount(value / usdRates)
    } else {
      setTokenInputAmount(value)
      setUsdInputAmount(value * usdRates)
    }
  }

  const handleSwap = () => {
    if (depositCurrency === "USD") {
      setDepositCurrency(token)
    } else {
      setDepositCurrency("USD")
    }
  }

  useEffect(() => {
    inputChange(tokenInputAmount)
  }, [inputChange, tokenInputAmount])

  return (
    <Box className={classes.root}>
      <Box
        display="flex"
        justifyContent={"space-between"}
        alignItems={"center"}
      >
        <Typography component="h5" variant="h5">
          <small style={{ textTransform: "uppercase" }}>Deposit {token}</small>
        </Typography>
        <Typography component="h6" variant="h6">
          <small>
            Balance: {depositCurrency !== "USD" ? tokenBalance : usdBalance}{" "}
            {depositCurrency}
          </small>
        </Typography>
      </Box>
      <Box mb="5px" />
      <Box className={cx(classes.input)}>
        <input
          type="number"
          step=".01"
          placeholder={`0 ${token.toUpperCase()}`}
          value={depositCurrency === "USD" ? usdInputAmount : tokenInputAmount}
          onChange={handleInputChange}
        />
        <Typography component="h6" variant="h6">
          <small>
            ~ {depositCurrency === "USD" ? tokenInputAmount : usdInputAmount}{" "}
            {depositCurrency === "USD" ? token : "USD"}
          </small>
        </Typography>

        <SwapIcon className="swap" onClick={handleSwap} />
      </Box>
    </Box>
  )
}

export default AmountInput
