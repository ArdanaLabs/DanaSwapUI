import React, { ChangeEvent, useState } from "react"
import cx from "classnames"
import { Box, useMediaQuery, Typography } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"

import { printCurrencyUSD } from "hooks"
import { USD } from "Data/Unit"
import { Currency } from "pages/Swap/Swap"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: palette.type === "light" ? "transparent" : "#0A104599",
    borderRadius: "5px",
    padding: 15,
    boxShadow:
      palette.type === "light"
        ? "2px 2px 10px rgba(0, 0, 0, 0.1)"
        : "0px 4px 4px rgba(0, 0, 0, 0.25)",
  },

  label: {
    color: palette.secondary.main,
    textTransform: "uppercase",
  },

  amount: {
    color: palette.primary.main,
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontSize: "30px",
    outline: "none",
    border: "none",
    background: "none",
    width: "100%",

    [`&::-webkit-outer-spin-button, &::-webkit-inner-spin-button`]: {
      [`-webkit-appearance`]: "none",
      margin: 0,
    },

    [breakpoints.down("xs")]: {
      fontSize: 24,
    },
  },

  tokenAvatar: {
    display: "flex",
    alignItems: "center",

    [`& > div`]: {
      margin: "0 10px",
    },
  },

  maxButton: {
    background: `linear-gradient(90deg, ${palette.secondary.main} 0%, ${palette.secondary.dark} 100%)`,
    borderRadius: "50px",
    textAlign: "center",
    color: palette.common.white,
    padding: "5px 15px",
    cursor: "pointer",
    textTransform: "uppercase",
  },

  token: {
    display: "flex",
    alignItems: "center",
    cursor: "pointer",
    width: "90px",
  },

  tokenImage: {
    marginRight: "10px",
    width: "30px",
    height: "30px",
  },

  tokenDenom: {
    color: palette.primary.main,
    [`& h6`]: {
      fontWeight: "normal",
    },
  },

  filterText: {
    background: palette.common.white,
    fontSize: "10px",
    fontWeight: 500,
    lineHeight: "100%",
    width: "100%",
    padding: "15px 20px",
    borderRadius: "10px",
    color: palette.type === "light" ? palette.primary.main : palette.text.hint,

    [`&::placeholder`]: {
      color:
        palette.type === "light" ? palette.primary.main : palette.text.hint,
    },
  },

  filterType: {
    background: "transparent",
    padding: "5px 10px",
    fontSize: "10px",
    lineHeight: "100%",
    margin: "20px 10px 20px 0px",
    border: `1px solid ${palette.secondary.main}`,
    color: palette.text.secondary,
  },

  active: {
    background: palette.primary.light,
    border: "unset",
    color: palette.common.white,
  },

  menuItem: {
    borderBottom: "1px solid white",
  },
}))

export interface OverViewBoxProps {
  label: string
  amount: number
  token: Currency
  handleOpenSelectAssetModal: () => void
  handleAmountChange: (e: number) => void
}

const TokenBox: React.FC<OverViewBoxProps> = ({
  label,
  amount,
  token,
  handleAmountChange,
  handleOpenSelectAssetModal,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const [balance] = useState(100)
  const [useRate] = useState(1.22)

  return (
    <Box className={classes.root}>
      <Typography variant="h4" component="h4" className={cx(classes.label)}>
        {label}&nbsp;(
        {printCurrencyUSD(USD.iso.wrap(amount * useRate), {
          minimumFractionDigits: 2,
        })}
        )
      </Typography>
      <Box display="flex" justifyContent="between" alignItems="center">
        <input
          className={cx(classes.amount)}
          type="number"
          step={0.1}
          min={0}
          value={amount}
          onChange={(e: ChangeEvent<HTMLInputElement>): void =>
            handleAmountChange(Number(e.target.value))
          }
        />
        <Box className={cx(classes.tokenAvatar)}>
          <Box
            id="max_button"
            className={cx(classes.maxButton)}
            onClick={(): void => handleAmountChange(balance)}
          >
            <Typography variant="h6" component="h6">
              Max
            </Typography>
          </Box>
          <Box className={classes.token} onClick={handleOpenSelectAssetModal}>
            <img
              className={classes.tokenImage}
              src={token.imageUrl}
              alt="token"
            />
            <Box className={cx(classes.tokenDenom)}>
              <Typography component="h4" variant="h4">
                {token.denom}
              </Typography>
              <Typography component="h6" variant="h6">
                {token.minimalDenom}
              </Typography>
            </Box>
          </Box>
        </Box>
      </Box>
    </Box>
  )
}

export default TokenBox
