import React from "react"
import { Box, Fade, Grid, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import cx from "classnames"

import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"
import { Slider } from "components"
import { Radio } from "components/Button"

import LOGO_Ardana from "assets/coins/ardana.png"

const options = [
  {
    title: "Slippage",
    data: [
      { label: "0.5%", value: "0.5" },
      { label: "1%", value: "1" },
      { label: "", value: "Custom", hasInput: true },
    ],
  },
  {
    title: "Gas Prices",
    data: [
      { label: "20.5 Slow", value: "20.5" },
      { label: "25 Standard", value: "25" },
      { label: "28 Fast", value: "28" },
      { label: "32 Instant", value: "32" },
      { label: "", value: "Custom", hasInput: true },
    ],
  },
]
const exOptions = [
  {
    title: "",
    data: [
      { label: "Add all coins in an equal proportion", value: "proportion" },
      { label: "Deposit maximum amount of available coins", value: "maximum" },
    ],
  },
]

const currencies = [
  {
    name: "DANA",
    desc: "exDANA",
    src: LOGO_Ardana,
    amount: 0,
  },
  {
    name: "DANA",
    desc: "exDANA",
    src: LOGO_Ardana,
    amount: 0,
  },
  {
    name: "DANA",
    desc: "exDANA",
    src: LOGO_Ardana,
    amount: 0,
  },
]

const useStyles = makeStyles(({ palette }) => ({
  root: {
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
  },

  card: {
    background: palette.background.paper,
    boxShadow: "2px 2px 10px rgba(0, 0, 0, 0.1)",
    borderRadius: "5px",
  },

  secondFont: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    lineHeight: "100%",
  },

  label: {
    fontSize: "15px",
    fontWeight: 500,
    paddingBottom: "10px",
    color: palette.text.primary,
  },
  content: {
    fontSize: "17px",
    fontWeight: 300,
    color: palette.text.secondary,
  },

  tokenIcon: {
    "background": palette.common.white,
    "borderRadius": "50%",
    "padding": "10px",
    "display": "flex",
    "justifyContent": "center",
    "alignItems": "center",

    "& img": {
      width: "20px",
      height: "20px",
    },
  },

  tokenName: {
    "fontFamily": "Museo Sans",
    "fontStyle": "normal",
    "lineHeight": "100%",
    "color": palette.secondary.main,
    "flexGrow": 2,

    "& span:first-child": {
      fontSize: "15px",
      fontWeight: 500,
    },

    "& span:last-child": {
      fontSize: "10px",
      fontWeight: 500,
    },
  },

  submit: {
    "background": "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
    "borderRadius": "20px",
    "fontFamily": "Museo Sans",
    "fontStyle": "normal",
    "fontWeight": "bold",
    "fontSize": "24px",
    "lineHeight": "100%",
    "textAlign": "center",
    "color": palette.common.white,
    "padding": "15px 30px",
    "cursor": "pointer",
    "margin": "0px 20px",

    "&:hover": {
      background: "linear-gradient(-90deg, #5F72FF 0%, #73D6F1 100%)",
    },
  },
}))

const Deposit: React.FC = () => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

  return (
    <Fade in={true}>
      <Box className={cx(classes.root)}>
        <Box className={cx(classes.card)} p="50px 20px" borderRadius="10px">
          <Grid container>
            <Grid item xs={12} sm={6}>
              <Box p="20px" width="400px" height="100%">
                {currencies &&
                  currencies.map((currency: any, i: number) => (
                    <Box key={i} marginBottom="20px">
                      <Box
                        className={cx(classes.card)}
                        p="15px"
                        display="flex"
                        justifyContent="space-between"
                        alignItems="center"
                      >
                        <Box display="flex" alignItems="center">
                          <Box className={cx(classes.tokenIcon)}>
                            <img src={currency.src} alt="token" width="30px" />
                          </Box>
                          <Box pl="10px" className={cx(classes.tokenName)}>
                            <span>{currency.name}</span>
                            <br />
                            <span>{currency.desc}</span>
                          </Box>
                        </Box>
                        <Box>
                          <Box
                            className={cx(classes.secondFont, classes.label)}
                          >
                            AMOUNT
                          </Box>
                          <Box
                            className={cx(classes.secondFont, classes.content)}
                          >
                            {currency.amount}
                          </Box>
                        </Box>
                      </Box>
                      <Box mt="10px"></Box>
                      <Box paddingRight="100px">
                        <Box className={cx(classes.secondFont, classes.label)}>
                          DEPOSIT
                        </Box>
                        <Slider min={0} max={100} defaultValue={0} step={1} />
                      </Box>
                    </Box>
                  ))}
              </Box>
            </Grid>
            <Grid item xs={12} sm={6}>
              <Box
                p="20px"
                height="100%"
                width="400px"
                borderLeft="1px solid #C4C4C4"
              >
                <Grid container spacing={mobile ? 1 : 2}>
                  {exOptions.map((option, i) => (
                    <Grid container item xs={12} key={i}>
                      <Radio option={option} value={option.data[0].value} />
                    </Grid>
                  ))}
                </Grid>
                <Box mt="30px" />
                <Grid
                  container
                  spacing={mobile ? 1 : 2}
                  style={{ marginTop: "10px" }}
                >
                  {options.map((option, i) => (
                    <Grid container item xs={6} key={i}>
                      <Radio option={option} value={option.data[0].value} />
                    </Grid>
                  ))}
                </Grid>
              </Box>
            </Grid>
          </Grid>

          <Box
            display="flex"
            justifyContent="center"
            pt="20px"
            alignItems="center"
          >
            <Box className={cx(classes.submit)}>DEPOSIT</Box>
            <Box className={cx(classes.submit)}>DEPOSIT & STAKE IN AREM</Box>
          </Box>
        </Box>
      </Box>
    </Fade>
  )
}

export default Deposit
