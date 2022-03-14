import React, { useEffect, useState } from "react"
import { Box, CircularProgress, Grid, useMediaQuery } from "@material-ui/core"
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
import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"
import { ApexOptions } from "apexcharts"
import { useAggVolume, useAggLiquidity } from "state/chart/hooks"
import { extractDateAxis, printCurrencyUSD, printDate } from "hooks"

const useStyles = makeStyles(({ palette }) => ({
  self: {
    background: "unset",
  },

  title: {
    color: palette.text.primary,
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "28px",
    lineHeight: "110%",
  },

  panel: {
    background: palette.background.paper,
    borderRadius: "10px",
    padding: "30px 20px",
    filter: "drop-shadow(2px 2px 10px rgba(0, 0, 0, 0.1))",
  },

  panelFilter: {
    "display": "flex",
    "justifyContent": "space-between",
    "color": palette.text.hint,
    "fontFamily": "Museo Sans",
    "fontStyle": "normal",
    "fontWeight": 500,
    "fontSize": "11px",
    "lineHeight": "100%",
    "paddingBottom": "30px",

    "& span": {
      padding: "10px",
      cursor: "pointer",
    },
  },

  panelFilterByType: {},

  panelFilterByDate: {},
}))

const ChartSection: React.FC = () => {
  const { palette, breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const isDarkTheme: boolean = Theme.Eq.equals(userTheme, Theme.Theme.Dark)
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark: isDarkTheme, mobile })

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
      /* TODO: text-transform: uppercase */
      /* TODO: Intl.DateFormat */
      categories: ["APR 20", "MAY 15", "JUN 02"],
      labels: {
        show: false,
        style: {
          colors: palette.text.hint,
          fontSize: "11px",
          fontFamily: "Museo Sans",
          fontWeight: 500,
        },
      },
      tickPlacement: "between",
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
        align: "left",
        style: {
          colors: palette.secondary.main,
          fontFamily: "Museo Sans",
          fontWeight: "bold",
          fontSize: "16px",
          cssClass: "apexcharts-yaxis-label",
        },
        formatter: (n) => printCurrencyUSD(USD.iso.wrap(n)),
      },
    },
    grid: {
      show: false,
    },
    fill: {
      type: "gradient",
      colors: [isDarkTheme ? "#73d6f1" : "#202F9A"],
      gradient: {
        type: "vertical", // The gradient in the horizontal direction
        gradientToColors: [isDarkTheme ? "#73D6F1" : "#5F72FF"], // The color at the end of the gradient
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
              chart: {
                id: "chart-agg-liquidity",
              },
              xaxis: {
                categories: extractDateAxis(agls).map((d: Date): string =>
                  printDate(d)
                ),
                labels: {
                  show: true,
                },
              },
              yaxis: {
                labels: {
                  show: true,
                  align: "left",
                  style: {
                    colors: palette.secondary.main,
                    fontFamily: "Museo Sans",
                    fontWeight: "bold",
                    fontSize: "16px",
                    cssClass: "apexcharts-yaxis-label",
                  },
                  formatter: (usd: any): string => printCurrencyUSD(usd),
                },
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
              chart: {
                id: "chart-agg-volume",
              },
              xaxis: {
                categories: extractDateAxis(agvs).map((d: Date): string =>
                  printDate(d)
                ),
                labels: {
                  show: true,
                },
              },
              yaxis: {
                labels: {
                  show: true,
                  align: "left",
                  style: {
                    colors: palette.secondary.main,
                    fontFamily: "Museo Sans",
                    fontWeight: "bold",
                    fontSize: "16px",
                    cssClass: "apexcharts-yaxis-label",
                  },
                  formatter: (usd: any): string => printCurrencyUSD(usd),
                },
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
      ...volumeOptions,
      chart: {
        id: "chart-agg-volume",
      },
      xaxis: {
        labels: {
          style: {
            colors: palette.text.hint,
          },
        },
      },
      yaxis: {
        labels: {
          show: true,
          align: "left",
          style: {
            colors: palette.secondary.main,
            fontFamily: "Museo Sans",
            fontWeight: "bold",
            fontSize: "16px",
            cssClass: "apexcharts-yaxis-label",
          },
          formatter: (n: number): string => printCurrencyUSD(USD.iso.wrap(n)),
        },
      },
      fill: {
        colors: [isDarkTheme ? "#73d6f1" : "#202F9A"],
        gradient: {
          gradientToColors: [isDarkTheme ? "#73D6F1" : "#5F72FF"],
        },
      },
    })
    setLiquidityOptions({
      ...liquidityOptions,
      chart: {
        id: "chart-agg-liquidity",
      },
      xaxis: {
        labels: {
          style: {
            colors: palette.text.hint,
          },
        },
      },
      yaxis: {
        labels: {
          show: true,
          align: "left",
          style: {
            colors: palette.secondary.main,
            fontFamily: "Museo Sans",
            fontWeight: "bold",
            fontSize: "16px",
            cssClass: "apexcharts-yaxis-label",
          },
          formatter: (n: number): string => printCurrencyUSD(USD.iso.wrap(n)),
        },
      },
      fill: {
        colors: [isDarkTheme ? "#73d6f1" : "#202F9A"],
        gradient: {
          gradientToColors: [isDarkTheme ? "#73D6F1" : "#5F72FF"],
        },
      },
    })
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [isDarkTheme])

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
            position="absolute"
            top={0}
            left={0}
            width="100%"
            height="100%"
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
            position="absolute"
            top={0}
            left={0}
            width="100%"
            height="100%"
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
    <Box className={cx(classes.self)}>
      <Grid container spacing={3}>
        <Grid item sm={12} md={6} style={{ width: "100%" }}>
          <Box className={cx(classes.title)}>Volume</Box>
          <Box mt="20px" />
          <Box className={cx(classes.panel)}>
            <Box className={cx(classes.panelFilter)}>
              <Box className={cx(classes.panelFilterByType)}>
                {/* TODO: text-transform: uppercase */}
                <Box component="span">TOTAL</Box>
                <Box component="span">SWAP</Box>
                <Box component="span">ADD</Box>
                <Box component="span">WITHDRAW</Box>
              </Box>
              <Box className={cx(classes.panelFilterByDate)}>
                {/* TODO: text-transform: uppercase */}
                <Box component="span">WEEK</Box>
                <Box component="span">ALL</Box>
              </Box>
            </Box>
            <Box position="relative">{renderAggVolumeChart(aggVolume)}</Box>
          </Box>
        </Grid>
        <Grid item sm={12} md={6} style={{ width: "100%" }}>
          <Box className={cx(classes.title)}>Liquidity</Box>
          <Box mt="20px" />
          <Box className={cx(classes.panel)}>
            <Box className={cx(classes.panelFilter)}>
              <Box className={cx(classes.panelFilterByType)}>
                {/* TODO: text-transform: uppercase */}
                <Box component="span">LIQUIDITY</Box>
                <Box component="span">LP EARNING</Box>
                <Box component="span">BOND EARNING</Box>
                <Box component="span">$RUNE PRICE</Box>
              </Box>
              <Box className={cx(classes.panelFilterByDate)}>
                <Box component="span">WEEK</Box>
                <Box component="span">ALL</Box>
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
