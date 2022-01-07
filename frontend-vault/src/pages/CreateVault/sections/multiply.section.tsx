import React, { useState } from "react"
import { useHistory, useParams } from "react-router-dom"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Typography,
  Button,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"

import { ReactComponent as CircleQuestionIcon } from "assets/image/icons/circle-question.svg"
import { currencyFormatter, numberFormatter, percentageFormatter } from "hooks"
import { AmountInput, Slider } from "components"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    [`& .MuiGrid-item`]: {
      padding: "8px",

      [`&.MuiGrid-container`]: {
        padding: 0,
      },
    },
  },
  card: {
    background:
      palette.type === "light"
        ? "linear-gradient(359deg, #B9D4FF -129.98%, #FFFFFF 99.14%)"
        : palette.background.paper,
    borderRadius: "20px",
    padding: "25px",
    height: "100%",
    // margin: "5px",
    // minHeight: "180px",

    [`& h5`]: {
      textTransform: "uppercase",
    },
    [`& h1, & h3, & h5, & h6`]: {
      color: palette.primary.main,
    },
  },
  badge: {
    borderRadius: "100px",
    padding: "5px 15px",
    background: palette.error.light,
    color: `${palette.error.main} !important`,
    display: "inline-block",
    fontWeight: 600,
  },
  questionIcon: {
    width: "20px",
    height: "20px",

    [`& path`]: {
      fill: palette.primary.main,
    },
    [`& rect`]: {
      stroke: palette.primary.main,
    },
  },
  highlight: {
    color: `${palette.info.dark} !important`,
  },
  warning: {
    color: `${palette.warning.main} !important`,
  },
  button: {
    borderRadius: "50px",
    width: "100%",
    padding: "10px",
    background: palette.info.light,

    [`& h5`]: {
      textTransform: "capitalize",
      color: palette.common.white,
    },

    [`&.disabled`]: {
      background: "#AFAFAF",
      pointerEvents: "none",
    },
  },
  divider: {
    background: palette.info.dark,
    width: "100%",
    height: "1px",
    marginTop: "30px",
    marginBottom: "30px",
  },
  alert: {
    background: palette.error.light,
    padding: "20px",
    borderRadius: "10px",

    [`& h6`]: {
      color: `${palette.error.main} !important`,
    },
  },
}))

const MultiplySection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const { type } = useParams<{ type: string }>()
  const history = useHistory()
  const [errMsgs] = useState([
    "You cannot deposit more collateral than the amount in your wallet.",
  ])

  const token = "ETH"
  const [amount, setAmount] = useState<number>(0)

  const liquidationPrice = 2.32
  const liquidationPriceAfter = 4324.67

  const buyingPower = 5.22
  const buyingPowerAfter = 4424.67

  const currentPrice = 4795.23
  const nextPrice = 4959.42
  const increasedPercent = 0

  const netValue = 0
  const netValueAfter = 4959.42

  const debt = 0
  const debtAfter = 2

  const withdraw = 0
  const withdrawAfter = 2.31

  const generate = 9
  const generateAfter = 3.42

  const handleSetupProxy = () => {
    console.log("handleSetupProxy")
  }

  const handleSwitchToBorrow = () => {
    console.log("handleSwitchToBorrow")
    history.push(`/vaults/open-borrow/${type}`)
  }

  const renderLiquidationPrice = () => (
    <Box className={cx(classes.card)}>
      <Box
        display={"flex"}
        justifyContent={"space-between"}
        alignItems={"center"}
        mb={"10px"}
      >
        <Typography component="h5" variant="h5">
          Liquidation Price
        </Typography>
        <CircleQuestionIcon className={cx(classes.questionIcon)} />
      </Box>
      <Box mb={"15px"}>
        <Typography component="h1" variant="h1">
          {currencyFormatter(liquidationPrice)}
        </Typography>
      </Box>

      <Typography component="h6" variant="h6" className={cx(classes.badge)}>
        {currencyFormatter(liquidationPriceAfter)} after
      </Typography>
    </Box>
  )

  const renderBuyingPower = () => (
    <Box className={cx(classes.card)}>
      <Box
        display={"flex"}
        justifyContent={"space-between"}
        alignItems={"center"}
        mb={"10px"}
      >
        <Typography component="h5" variant="h5">
          Buying Power
        </Typography>
        <CircleQuestionIcon className={cx(classes.questionIcon)} />
      </Box>
      <Box mb={"15px"}>
        <Typography component="h1" variant="h1">
          {currencyFormatter(buyingPower)}
        </Typography>
      </Box>

      <Typography component="h6" variant="h6" className={cx(classes.badge)}>
        {currencyFormatter(buyingPowerAfter)} after
      </Typography>
    </Box>
  )

  const renderCurrentPrice = () => (
    <Box className={cx(classes.card)}>
      <Box
        display={"flex"}
        justifyContent={"space-between"}
        alignItems={"center"}
        mb={"10px"}
      >
        <Typography component="h5" variant="h5">
          Current Price
        </Typography>
        <CircleQuestionIcon className={cx(classes.questionIcon)} />
      </Box>
      <Box mb={"15px"}>
        <Typography component="h1" variant="h1">
          {currencyFormatter(currentPrice)}
        </Typography>
      </Box>

      <Typography component="h6" variant="h6">
        Next:{" "}
        <strong className={cx(classes.highlight)}>
          {currencyFormatter(nextPrice)}
        </strong>{" "}
        <small>{percentageFormatter(increasedPercent)}</small>
      </Typography>
    </Box>
  )

  const renderNetValue = () => (
    <Box className={cx(classes.card)}>
      <Box
        display={"flex"}
        justifyContent={"space-between"}
        alignItems={"center"}
        mb={"10px"}
      >
        <Typography component="h5" variant="h5">
          Net Value
        </Typography>
        <CircleQuestionIcon className={cx(classes.questionIcon)} />
      </Box>
      <Box mb={"15px"}>
        <Typography component="h1" variant="h1">
          {currencyFormatter(netValue)}
        </Typography>
      </Box>

      <Typography component="h6" variant="h6" className={cx(classes.badge)}>
        {currencyFormatter(netValueAfter)} after
      </Typography>
    </Box>
  )

  const renderVaultStat = () => (
    <Box className={cx(classes.card)}>
      <Grid container>
        <Grid item xs={12} md={4}>
          <Typography component="h5" variant="h5">
            Vault DAI Debt
          </Typography>
          <Box mb="10px" />
          <Typography component="h6" variant="h6">
            {numberFormatter(debt)} DAI
          </Typography>
          <Box mb="15px" />
          <Typography component="h6" variant="h6" className={cx(classes.badge)}>
            {currencyFormatter(debtAfter)} after
          </Typography>
        </Grid>
        <Grid item xs={12} md={4}>
          <Typography component="h5" variant="h5">
            Total ETH Exposure
          </Typography>
          <Box mb="10px" />
          <Typography component="h6" variant="h6">
            {numberFormatter(withdraw)} ETH
          </Typography>
          <Box mb="15px" />
          <Typography component="h6" variant="h6" className={cx(classes.badge)}>
            {numberFormatter(withdrawAfter)} after
          </Typography>
        </Grid>
        <Grid item xs={12} md={4}>
          <Typography component="h5" variant="h5">
            Multiply
          </Typography>
          <Box mb="10px" />
          <Typography component="h6" variant="h6">
            {numberFormatter(generate)}x
          </Typography>
          <Box mb="15px" />
          <Typography component="h6" variant="h6" className={cx(classes.badge)}>
            {numberFormatter(generateAfter)} after
          </Typography>
        </Grid>
      </Grid>
    </Box>
  )

  const renderConfig = () => {
    const renderUpdateInfo = (
      name: string,
      value: string,
      tip: string = ""
    ) => (
      <Box
        display={"flex"}
        justifyContent={"space-between"}
        alignItems={"center"}
        mb="10px"
      >
        <Typography component="h5" variant="h5">
          <small>{name}</small>
        </Typography>
        <Typography component="h6" variant="h6">
          <small>
            <b>{value}</b> {tip && tip}
          </small>
        </Typography>
      </Box>
    )
    return (
      <Box className={cx(classes.card)}>
        <Box display="flex" justifyContent="space-between" alignItems="center">
          <Typography component="h3" variant="h3">
            Configure your Vault
          </Typography>
          <Typography
            component="h5"
            variant="h5"
            className={cx(classes.highlight)}
          >
            1/3
          </Typography>
        </Box>
        <Box mb="10px" />
        <Typography component="h6" variant="h6">
          Simulate your vault by configuring the amount of collateral to
          deposit, and slide your multiply factor.
        </Typography>
        <Box mb="20px" />
        <Typography component="h3" variant="h3">
          Deposit your ETH
        </Typography>
        <Box mb="10px" />
        <Box>
          <AmountInput token={token} inputChange={setAmount} />
        </Box>
        <Box mb="20px" />
        <Typography component="h3" variant="h3">
          Adjust your Multiply
        </Typography>
        <Box mb="20px" />
        <Box display={"flex"} justifyContent={"space-between"}>
          <Box>
            <Typography component="h5" variant="h5">
              <small style={{ textTransform: "uppercase" }}>
                Deposit {token}
              </small>
            </Typography>
            <Typography
              component="h3"
              variant="h3"
              className={classes.highlight}
            >
              <big>{currencyFormatter(amount * currentPrice)}</big>
            </Typography>
          </Box>
          <Box>
            <Typography component="h5" variant="h5">
              <small style={{ textTransform: "uppercase" }}>
                Collateral Ratio
              </small>
            </Typography>
            <Typography component="h3" variant="h3" className={classes.warning}>
              <big>{percentageFormatter(1.45)}</big>
            </Typography>
          </Box>
        </Box>

        <Box mb="20px" />
        <Slider min={0} max={100} defaultValue={0} step={1} />
        <Box mb="10px" />
        <Box display="flex" justifyContent={"space-between"}>
          <Typography component="h6" variant="h6" style={{ fontWeight: 100 }}>
            Decrease Risk
          </Typography>
          <Typography component="h6" variant="h6" style={{ fontWeight: 100 }}>
            Increase Risk
          </Typography>
        </Box>

        <Box className={cx(classes.divider)} />
        <Typography component="h3" variant="h3">
          Order Information
        </Typography>
        <Box mb="10px" />
        <Box>
          {renderUpdateInfo(
            `BUYING ${token}`,
            `2.2041 ${token}`,
            `(${currencyFormatter(10518.4)})`
          )}
          {renderUpdateInfo(
            `TOTAL ${token} EXPOSURE`,
            `0.00 ${token} → 2.2041 ${token}`
          )}
          {renderUpdateInfo(
            `${token} PRICE(impact)`,
            currencyFormatter(4784.31),
            `(${currencyFormatter(10518.4)})`
          )}
          {renderUpdateInfo(`SLIPPAGE LIMIT`, percentageFormatter(0.005))}
          {renderUpdateInfo(
            `MULTIPLE`,
            `${numberFormatter(0)}x → ${numberFormatter(3.22)}x`
          )}
          {renderUpdateInfo(
            `OUTSTANDING DEBT`,
            `${numberFormatter(0)} DAI → ${numberFormatter(10456.48)} DAI`
          )}
          {renderUpdateInfo(
            `COLLATERAL RATIO`,
            `${percentageFormatter(0)} → `,
            `${percentageFormatter(1.45)}`
          )}
          {renderUpdateInfo(`FEES + (max gas fee)`, `${21.07} + `, `(n/a)`)}
        </Box>
        <Box mb="15px" />
        <Box className={cx(classes.alert)}>
          <Box component="ul" paddingLeft={"20px"}>
            {errMsgs.map((msg, index) => (
              <Typography component="h6" variant="h6">
                <li key={index}>{msg}</li>
              </Typography>
            ))}
          </Box>
        </Box>
        <Box mb="20px" />
        <Button
          className={cx(classes.button, { disabled: true })}
          onClick={handleSetupProxy}
        >
          <Typography
            component="h5"
            variant="h5"
            style={{ textTransform: "uppercase" }}
          >
            Setup Proxy
          </Typography>
        </Button>
        <Box className={cx(classes.divider)} />
        <Button className={cx(classes.button)} onClick={handleSwitchToBorrow}>
          <Typography
            component="h5"
            variant="h5"
            style={{ textTransform: "uppercase" }}
          >
            Switch to Borrow
          </Typography>
        </Button>
      </Box>
    )
  }

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container spacing={0} alignItems={"flex-start"}>
          <Grid
            item
            container
            spacing={0}
            xs={12}
            md={8}
            alignItems={"stretch"}
          >
            <Grid item xs={12} md={6}>
              {renderLiquidationPrice()}
            </Grid>
            <Grid item xs={12} md={6}>
              {renderBuyingPower()}
            </Grid>
            <Grid item xs={12} md={6}>
              {renderCurrentPrice()}
            </Grid>
            <Grid item xs={12} md={6}>
              {renderNetValue()}
            </Grid>
            <Grid item xs={12}>
              {renderVaultStat()}
            </Grid>
          </Grid>
          <Grid item xs={12} md={4}>
            {renderConfig()}
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default MultiplySection
