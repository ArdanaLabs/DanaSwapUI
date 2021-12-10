import React, { useEffect, useState } from "react"
import { Box, Grid, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import Chart from "react-apexcharts"

import { useIsDarkMode } from "state/user/hooks"
import { ApexOptions } from "apexcharts"
import { useLocation } from "react-router-dom"
import { usePoolStats } from "state/home/hooks"
import { usePoolAPY, usePoolFees, usePoolTxCount } from "state/chart/hooks"
import { OneWeek } from "config/grains"
import { extractYAxis } from "hooks"

const useStyles = makeStyles(({ palette }) => ({
  self: {
    background: "unset",
  },

  title: {
    color: palette.text.primary,
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    marginTop: "20px",
    fontSize: "18px",
    lineHeight: "110%",
  },

  panel: {
    background:
      palette.type === "light"
        ? palette.common.white
        : palette.background.paper,
    borderRadius: "10px",
    padding: "20px",
    height: "calc(100% - 70px)",

    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontSize: "12px",
    lineHeight: "115%",
    fontWeight: 100,
    color: palette.secondary.main,
  },
}))

const ChartSection: React.FC = () => {
  const { palette, breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const location = useLocation()
  const poolStats = usePoolStats()
  const { poolAPY, getPoolAPY } = usePoolAPY()
  const { poolFees, getPoolFees } = usePoolFees()
  const { poolTXCount, getPoolTXCount } = usePoolTxCount()

  let options: ApexOptions = {
    chart: {
      id: "basic-bar",
      zoom: {
        enabled: false,
      },
      toolbar: {
        show: false,
      },
    },
    stroke: {
      width: 0,
      curve: "smooth",
    },
    xaxis: {
      labels: {
        show: false,
      },
      axisTicks: {
        show: false,
      },
      axisBorder: {
        show: false,
      },
    },
    yaxis: {
      labels: {
        show: false,
      },
    },
    grid: {
      show: false,
    },
    fill: {
      type: "gradient",
      colors: [!dark ? "#202F9A" : "#73d6f1"],
      gradient: {
        type: "vertical", // The gradient in the horizontal direction
        gradientToColors: [!dark ? "#5F72FF" : "#73D6F1"], // The color at the end of the gradient
        opacityFrom: 1, // transparency
        opacityTo: 0.3,
        stops: [0, 1200],
      },
    },
    plotOptions: {
      bar: {
        borderRadius: 5,
      },
    },
    dataLabels: {
      enabled: false,
    },
    tooltip: {
      enabled: false,
    },
  }

  const series = [
    {
      name: "series-1",
      data: [10],
    },
  ]

  const [poolInfo, setPoolInfo] = useState<any>(null)
  const [reserves, setReserves] = useState<any[]>([])
  const [currencySUM, setCurrencySUM] = useState({ label: "", value: 0 })
  const [APYChartOptions, setAPYChartOptions] = useState<ApexOptions>(options)
  const [APYChartSeries, setAPYChartSeries] = useState<any[]>(series)
  const [FeesChartOptions, setFeesChartOptions] = useState<ApexOptions>(options)
  const [FeesChartSeries, setFeesChartSeries] = useState<any[]>(series)
  const [TXChartOptions, setTXChartOptions] = useState<ApexOptions>(options)
  const [TXChartSeries, setTXChartSeries] = useState<any[]>(series)

  useEffect(() => {
    const { poolName }: any = location.state
    poolStats && poolStats[poolName] && setPoolInfo(poolStats[poolName])
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [poolStats, location.state])

  useEffect(() => {
    const { poolName }: any = location.state
    getPoolAPY(
      poolName,
      "2020-12-12T00:00:00.0Z",
      "2021-01-12T00:00:00.0Z",
      OneWeek
    )
    getPoolFees(
      poolName,
      "2020-12-12T00:00:00.0Z",
      "2021-01-12T00:00:00.0Z",
      OneWeek
    )
    getPoolTXCount(
      poolName,
      "2020-12-12T00:00:00.0Z",
      "2021-01-12T00:00:00.0Z",
      OneWeek
    )
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [location.state])

  useEffect(() => {
    if (poolInfo) {
      const currencyNames: string[] = Object.keys(poolInfo.reserves)
      const currencyValues: number[] = Object.values(poolInfo.reserves)

      const _currencySUM: number = currencyValues.reduce(
        (accumulator: number, current: number) => accumulator + current
      )

      const parsedReserves: any[] = currencyNames.map(
        (currencyName: string, index: number) => ({
          name: currencyName,
          value: currencyValues[index],
          ratio: ((100 * currencyValues[index]) / _currencySUM).toFixed(2),
        })
      )
      setReserves(parsedReserves)
      setCurrencySUM({
        label: currencyNames.join(" + "),
        value: _currencySUM,
      })
    }
  }, [poolInfo])

  useEffect(() => {
    if (!poolAPY) return
    setAPYChartOptions({
      ...APYChartOptions,
      chart: {
        id: "chart-pool-apy",
      },
      fill: {
        colors: [!dark ? "#202F9A" : "#73d6f1"],
        gradient: {
          gradientToColors: [!dark ? "#5F72FF" : "#73D6F1"],
        },
      },
    })
    setAPYChartSeries([
      {
        name: "APY",
        data: extractYAxis(poolAPY, "value"),
      },
    ])
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [poolAPY, palette])

  useEffect(() => {
    if (!poolFees) return
    setFeesChartOptions({
      ...FeesChartOptions,
      chart: {
        id: "chart-pool-fees",
      },
      fill: {
        colors: [!dark ? "#202F9A" : "#73d6f1"],
        gradient: {
          gradientToColors: [!dark ? "#5F72FF" : "#73D6F1"],
        },
      },
    })
    setFeesChartSeries([
      {
        name: "Fees",
        data: extractYAxis(poolFees, "value"),
      },
    ])
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [poolFees, palette])

  useEffect(() => {
    if (!poolTXCount) return
    setTXChartOptions({
      ...TXChartOptions,
      chart: {
        id: "chart-pool-txcount",
      },
      fill: {
        colors: [!dark ? "#202F9A" : "#73d6f1"],
        gradient: {
          gradientToColors: [!dark ? "#5F72FF" : "#73D6F1"],
        },
      },
    })
    setTXChartSeries([
      {
        name: "TxCount",
        data: extractYAxis(poolTXCount, "total"),
      },
    ])
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [poolTXCount, palette])

  return (
    <Box className={cx(classes.self)}>
      <Grid container spacing={3}>
        <Grid item sm={12} md={6} style={{ width: "100%" }}>
          <Box className={cx(classes.title)}>APY Graph</Box>
          <Box mt="20px" />
          <Box className={cx(classes.panel)}>
            <Chart
              options={APYChartOptions}
              series={APYChartSeries}
              type="area"
              width="100%"
            />
          </Box>
        </Grid>
        <Grid item sm={12} md={6} style={{ width: "100%" }}>
          <Box className={cx(classes.title)}>Historical Fee Data</Box>
          <Box mt="20px" />
          <Box className={cx(classes.panel)}>
            <Chart
              options={FeesChartOptions}
              series={FeesChartSeries}
              type="area"
              width="100%"
            />
          </Box>
        </Grid>
        <Grid item sm={12} md={6} style={{ width: "100%" }}>
          <Box className={cx(classes.title)}>Currency Reserves</Box>
          <Box mt="20px" />
          <Box className={cx(classes.panel)} padding="35px !important">
            {reserves &&
              reserves.map((currency: any, i: number) => (
                <Box component="p" key={i} margin={0}>
                  <b>{currency.name}:</b>&nbsp;{currency.value}&nbsp;(
                  {currency.ratio}%)
                </Box>
              ))}
            <b>{currencySUM.label}:</b>&nbsp;{currencySUM.value}
            <br />
            <b>USD total (NAV):</b> $
            {poolInfo && poolInfo.navUSD ? poolInfo.navUSD.toLocaleString() : 0}
            <br />
            <br />
            <b>Fee:</b>{" "}
            {poolInfo && poolInfo.feePercent
              ? poolInfo.feePercent.toLocaleString()
              : 0}
            %
            <br />
            <b>Admin fee:</b>{" "}
            {poolInfo && poolInfo.adminFeePercent
              ? poolInfo.adminFeePercent.toLocaleString()
              : 0}
            %
            <br />
            <br />
            <b>Virtual price:</b>{" "}
            {poolInfo && poolInfo.virtualPriceUSD
              ? poolInfo.virtualPriceUSD.toLocaleString()
              : 0}{" "}
            [?]
            <br />
            <b>A:</b>{" "}
            {poolInfo && poolInfo.amplificationCoefficient
              ? poolInfo.amplificationCoefficient.toLocaleString()
              : 0}
          </Box>
        </Grid>
        <Grid item sm={12} md={6} style={{ width: "100%" }}>
          <Box className={cx(classes.title)}>TX Graph</Box>
          <Box mt="20px" />
          <Box className={cx(classes.panel)}>
            <Chart
              options={TXChartOptions}
              series={TXChartSeries}
              type="area"
              width="100%"
            />
          </Box>
        </Grid>
      </Grid>
    </Box>
  )
}

export default ChartSection
