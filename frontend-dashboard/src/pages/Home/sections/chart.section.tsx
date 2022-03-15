import React, { useEffect, useState } from "react"
import {
  Box,
  CircularProgress,
  Grid,
  useMediaQuery,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import Chart from "react-apexcharts"

import * as NEA from "fp-ts/NonEmptyArray"
import * as O from "fp-ts/Option"
import * as RemoteData from "fp-ts-remote-data"

import { FetchDecodeError } from "Data/FetchDecode"
import * as LiquidityTokenPrice from "Data/LiquidityTokenPrice"
import * as Volume from "Data/Volume"
import { LiquidityChart, VolumeChart } from "Data/Chart"
import { Granularity } from "Data/Chart/Granularity"
import { USD } from "Data/Unit"
import { TransactionType } from "Data/Chart/TransactionType"
import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"
import { ApexOptions } from "apexcharts"
import { useAggVolume, useAggLiquidity } from "state/chart/hooks"
import { extractDateAxis, printCurrencyUSD, printDate } from "hooks"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: "unset",
  },

  title: {
    color: palette.primary.main,
    marginBottom: 50,
    [breakpoints.down("sm")]: { marginBottom: 30 },
  },

  panel: {
    background: `linear-gradient(126.33deg, ${palette.background.paper} 9.83%, #00000000 96.44%);`,
    borderRadius: "10px",
    padding: "30px 20px 0px",
    filter: "drop-shadow(2px 2px 10px rgba(0, 0, 0, 0.1))",

    [breakpoints.down("sm")]: {
      padding: "15px 10px 0px",
      marginBottom: 30,
    },
  },

  panelFilter: {
    display: "flex",
    alignItems: "center",
    color: palette.primary.main,
    paddingBottom: "30px",

    [`& h6`]: {
      padding: "10px",
      cursor: "pointer",
      textTransform: "uppercase",

      [breakpoints.down("sm")]: {
        padding: "5px",
        fontSize: 8,
      },
    },

    [breakpoints.down("sm")]: {
      paddingBottom: "10px",
    },
  },

  panelFilterByType: {
    display: "flex",
    [`& .active`]: {
      color: palette.secondary.main,
    },
  },

  panelFilterByDate: {
    display: "flex",
    [`& h6`]: {
      background: "transparent",
      borderRadius: "25px",
      padding: "6px 15px",

      [`&.active`]: {
        background: `linear-gradient(90deg, ${palette.secondary.dark} 0%, ${palette.secondary.main} 100%)`,
        color: palette.common.white,
      },

      [breakpoints.down("sm")]: {
        padding: "4px 10px",
      },
    },
  },
}))

const ChartSection: React.FC = () => {
  const { palette, breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const isDarkTheme: boolean = Theme.Eq.equals(userTheme, Theme.Theme.Dark)
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark: isDarkTheme, mobile })

  const [volumeChartFilter, setVolumeChartFilter] = useState({
    type: TransactionType.Any,
    date: Granularity.OneMonth,
  })

  const [liquidityChartFilter, setLiquidityChartFilter] = useState({
    date: Granularity.OneMonth,
  })

  const options: ApexOptions = {
    chart: {
      id: "basic-bar",
      zoom: {
        enabled: false,
      },
      toolbar: {
        show: true,
        tools: {
          download: false,
        },
      },
    },
    stroke: {
      width: 0,
      curve: "smooth",
    },
    xaxis: {
      /* TODO: Intl.DateFormat */
      categories: ["Apr 20", "May 15", "Jun 02"],
      labels: {
        show: true,
        style: {
          colors: palette.secondary.main,
          fontSize: !mobile ? "13px" : "8px",
          fontFamily: "Museo Sans, sans-serif",
          fontWeight: 600,
        },
        // formatter: (n) => printDate(n),
      },
      tickPlacement: "between",
      axisTicks: {
        show: false,
      },
      axisBorder: {
        show: true,
        color: palette.secondary.main,
      },
    },
    yaxis: {
      labels: {
        show: true,
        align: "left",
        style: {
          colors: palette.primary.main,
          fontFamily: "Museo Sans, sans-serif",
          fontWeight: 600,
          fontSize: !mobile ? "16px" : "10px",
        },
        formatter: (n) => printCurrencyUSD(USD.iso.wrap(n)),
      },
      axisBorder: {
        show: true,
        color: palette.secondary.main,
      },
    },
    grid: {
      show: false,
    },
    fill: {
      type: "gradient",
      colors: [palette.secondary.main],
      gradient: {
        type: "vertical", // The gradient in the horizontal direction
        gradientToColors: [palette.secondary.main], // The color at the end of the gradient
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
      data: [],
    },
  ]

  const [volumeOptions, setVolumeOptions] = useState<ApexOptions>(options)
  const [liquidityOptions, setLiquidityOptions] = useState<ApexOptions>(options)
  const [volumeSeries, setVolumeSeries] = useState<any[]>(series)
  const [liquiditySeries, setLiquiditySeries] = useState<any[]>(series)

  const { aggVolume, fetchAggVolume } = useAggVolume()
  const { aggLiquidity, fetchAggLiquidity } = useAggLiquidity()

  useEffect(() => {
    fetchAggLiquidity(
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2020-12-14T00:00:00.0Z")],
      Granularity.OneDay
    )()
    fetchAggVolume(
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2020-12-12T00:05:00.0Z")],
      Granularity.FiveMinutes
    )()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  useEffect(() => {
    switch (aggLiquidity._tag) {
      case "Success":
        O.fold(
          (): void => {},
          (agls: NEA.NonEmptyArray<LiquidityChart.Item.Type>): void => {
            setLiquidityOptions({
              ...liquidityOptions,
              xaxis: {
                ...liquidityOptions.xaxis,
                categories: extractDateAxis(agls).map((d: Date): string =>
                  printDate(d)
                ),
              },
            })
            setLiquiditySeries([
              {
                name: "Liquidity",
                data: agls.map(
                  ([, l]: LiquidityChart.Item.Type): USD.Type =>
                    LiquidityTokenPrice.iso.unwrap(l)
                ),
              },
            ])
          }
        )(NEA.fromArray(aggLiquidity.success))
        break
      case "Pending":
        break
      case "Failure":
        break
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [aggLiquidity])

  useEffect(() => {
    switch (aggVolume._tag) {
      case "Success":
        O.fold(
          () => {},
          (agvs: NEA.NonEmptyArray<VolumeChart.Item.Type>): void => {
            setVolumeOptions({
              ...volumeOptions,
              xaxis: {
                ...volumeOptions.xaxis,
                categories: extractDateAxis(agvs).map((d: Date): string =>
                  printDate(d)
                ),
              },
            })
            setVolumeSeries([
              {
                name: "Volume",
                data: agvs.map(([, v]: VolumeChart.Item.Type): USD.Type => {
                  // return USD.iso.unwrap(Volume.iso.unwrap(v.total))
                  return Volume.iso.unwrap(v.total)
                }),
              },
            ])
          }
        )(NEA.fromArray(aggVolume.success))
        break
      case "Pending":
        break
      case "Failure":
        break
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [aggVolume])

  useEffect(() => {
    setVolumeOptions({
      ...options,
    })
    setLiquidityOptions({
      ...options,
    })
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [palette, mobile])

  const handleVolumeChartFilterChange = (event: object) => {
    setVolumeChartFilter({ ...volumeChartFilter, ...event })
  }
  const handleLiquidityChartFilterChange = (event: object) => {
    setLiquidityChartFilter({ ...liquidityChartFilter, ...event })
  }

  function renderAggVolumeChart(
    agv: RemoteData.RemoteData<FetchDecodeError, VolumeChart.Type>
  ) {
    switch (agv._tag) {
      case "Success":
        return (
          <Chart
            options={volumeOptions}
            series={volumeSeries}
            type="bar"
            width="100%"
          />
        )
      case "Pending":
        return (
          <Box
            padding={"100px"}
            display="flex"
            justifyContent="center"
            alignItems="center"
          >
            <CircularProgress />
          </Box>
        )
      case "Failure":
        return (
          <details>
            <summary>TODO: ERROR Message</summary>
            <pre>{JSON.stringify(agv.failure, null, 2)}</pre>
          </details>
        )
    }
  }

  function renderAggLiquidityChart(
    agl: RemoteData.RemoteData<FetchDecodeError, LiquidityChart.Type>
  ) {
    switch (agl._tag) {
      case "Success":
        return (
          <Chart
            options={liquidityOptions}
            series={liquiditySeries}
            type="area"
            width="100%"
          />
        )
      case "Pending":
        return (
          <Box
            padding={"100px"}
            display="flex"
            justifyContent="center"
            alignItems="center"
          >
            <CircularProgress />
          </Box>
        )
      case "Failure":
        return (
          <details>
            <summary>TODO: ERROR Message</summary>
            <pre>{JSON.stringify(agl.failure, null, 2)}</pre>
          </details>
        )
    }
  }

  return (
    <Box className={cx(classes.root)}>
      <Grid container spacing={!mobile ? 5 : 2}>
        <Grid item sm={12} md={6} style={{ width: "100%" }}>
          <Typography variant="h1" component="h1" className={cx(classes.title)}>
            Volume
          </Typography>
          <Box className={cx(classes.panel)}>
            <Box
              className={cx(classes.panelFilter)}
              justifyContent="space-between"
            >
              <Box className={cx(classes.panelFilterByType)}>
                {[
                  [TransactionType.Any, "Total"],
                  [TransactionType.Trade, "Swap"],
                  [TransactionType.Deposit, "Add"],
                  [TransactionType.Withdrawal, "Withdraw"],
                ].map(([transactionType, label]) => (
                  <Typography
                    variant="h6"
                    component="h6"
                    key={label}
                    onClick={() =>
                      handleVolumeChartFilterChange({ type: transactionType })
                    }
                    className={cx({
                      active: volumeChartFilter.type === transactionType,
                    })}
                  >
                    {label}
                  </Typography>
                ))}
              </Box>
              <Box className={cx(classes.panelFilterByDate)}>
                {[
                  [Granularity.OneWeek, "Week"],
                  [Granularity.OneMonth, "All"],
                ].map(([granularity, label]) => (
                  <Typography
                    variant="h6"
                    component="h6"
                    key={label}
                    onClick={() =>
                      handleVolumeChartFilterChange({ date: granularity })
                    }
                    className={cx({
                      active: volumeChartFilter.date === granularity,
                    })}
                  >
                    {label}
                  </Typography>
                ))}
              </Box>
            </Box>
            <Box position="relative">{renderAggVolumeChart(aggVolume)}</Box>
          </Box>
        </Grid>
        <Grid item sm={12} md={6} style={{ width: "100%" }}>
          <Typography variant="h1" component="h1" className={cx(classes.title)}>
            Liquidity
          </Typography>
          <Box className={cx(classes.panel)}>
            <Box className={cx(classes.panelFilter)} justifyContent="flex-end">
              <Box className={cx(classes.panelFilterByDate)}>
                {[
                  [Granularity.OneWeek, "Week"],
                  [Granularity.OneMonth, "All"],
                ].map(([granularity, label]) => (
                  <Typography
                    variant="h6"
                    component="h6"
                    key={label}
                    onClick={() =>
                      handleLiquidityChartFilterChange({ date: granularity })
                    }
                    className={cx({
                      active: liquidityChartFilter.date === granularity,
                    })}
                  >
                    {label}
                  </Typography>
                ))}
              </Box>
            </Box>
            <Box position="relative">
              {renderAggLiquidityChart(aggLiquidity)}
            </Box>
          </Box>
        </Grid>
      </Grid>
    </Box>
  )
}

export default ChartSection
