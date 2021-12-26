import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"

import { ReactComponent as CircleQuestionIcon } from "assets/image/icons/circle-question.svg"
import { currencyFormatter, numberFormatter, percentageFormatter } from "hooks"

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
    [`& h1, & h5, & h6`]: {
      color: palette.primary.main,
    },
  },
  badge: {
    borderRadius: "100px",
    padding: "5px 15px",
    background: palette.success.light,
    color: `${palette.success.main} !important`,
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
    color: palette.info.dark,
  },
}))

const MainSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

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

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container spacing={0}>
          <Grid
            item
            container
            spacing={0}
            xs={12}
            md={8}
            alignItems={"stretch"}
          >
            <Grid item xs={12} md={6}>
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

                <Typography
                  component="h6"
                  variant="h6"
                  className={cx(classes.badge)}
                >
                  {currencyFormatter(liquidationPriceAfter)} after
                </Typography>
              </Box>
            </Grid>
            <Grid item xs={12} md={6}>
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

                <Typography
                  component="h6"
                  variant="h6"
                  className={cx(classes.badge)}
                >
                  {currencyFormatter(buyingPowerAfter)} after
                </Typography>
              </Box>
            </Grid>
            <Grid item xs={12} md={6}>
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
            </Grid>
            <Grid item xs={12} md={6}>
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

                <Typography
                  component="h6"
                  variant="h6"
                  className={cx(classes.badge)}
                >
                  {currencyFormatter(netValueAfter)} after
                </Typography>
              </Box>
            </Grid>
            <Grid item xs={12}>
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
                    <Typography
                      component="h6"
                      variant="h6"
                      className={cx(classes.badge)}
                    >
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
                    <Typography
                      component="h6"
                      variant="h6"
                      className={cx(classes.badge)}
                    >
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
                    <Typography
                      component="h6"
                      variant="h6"
                      className={cx(classes.badge)}
                    >
                      {numberFormatter(generateAfter)} after
                    </Typography>
                  </Grid>
                </Grid>
              </Box>
            </Grid>
          </Grid>
          <Grid item xs={12} md={4}>
            <Box className={cx(classes.card)}>6</Box>
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default MainSection
