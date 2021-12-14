import React, { useEffect, useState } from "react"
import { Box, CircularProgress, Grid, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import Chart from "react-apexcharts"

import { useIsDarkMode } from "state/user/hooks"
import { ApexOptions } from "apexcharts"
import { useAggVolume, useAggLiquidity } from "state/chart/hooks"
import { extractXAxis, extractYAxis, printCurrencyUSD } from "hooks"
import { FiveMinutes, OneDay } from "config/grains"

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
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

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
        formatter: (value: any) => {
          return printCurrencyUSD(value)
        },
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
      data: [],
    },
  ]

  const [volumeOptions, setVolumeOptions] = useState<ApexOptions>(options)
  const [liquidityOptions, setLiquidityOptions] = useState<ApexOptions>(options)
  const [volumeSeries, setVolumeSeries] = useState<any[]>(series)
  const [liquiditySeries, setLiquiditySeries] = useState<any[]>(series)

  const { aggVolume, getAggVolume } = useAggVolume()
  const { aggLiquidity, getAggLiquidity } = useAggLiquidity()

  useEffect(() => {
    getAggVolume(
      "2020-12-12T00:00:00.0Z",
      "2020-12-12T00:05:00.0Z",
      FiveMinutes
    )
    getAggLiquidity("2020-12-12T00:00:00.0Z", "2020-12-14T00:00:00.0Z", OneDay)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  useEffect(() => {
    if (aggVolume) {
      setVolumeOptions({
        ...volumeOptions,
        chart: {
          id: "chart-agg-volume",
        },
        xaxis: {
          categories: extractXAxis(aggVolume),
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
            formatter: (value: any) => {
              return printCurrencyUSD(value)
            },
          },
        },
      })
      setVolumeSeries([
        {
          name: "Volume",
          data: extractYAxis(aggVolume, "total"),
        },
      ])
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [aggVolume])

  useEffect(() => {
    if (aggLiquidity) {
      setLiquidityOptions({
        ...liquidityOptions,
        chart: {
          id: "chart-agg-liquidity",
        },
        xaxis: {
          categories: extractXAxis(aggLiquidity),
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
            formatter: (value: any) => {
              return printCurrencyUSD(value)
            },
          },
        },
      })
      setLiquiditySeries([
        {
          name: "Liquidity",
          data: extractYAxis(aggLiquidity, "value"),
        },
      ])
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [aggLiquidity])

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
          formatter: (value: any) => {
            return printCurrencyUSD(value)
          },
        },
      },
      fill: {
        colors: [!dark ? "#202F9A" : "#73d6f1"],
        gradient: {
          gradientToColors: [!dark ? "#5F72FF" : "#73D6F1"],
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
          formatter: (value: any) => {
            return printCurrencyUSD(value)
          },
        },
      },
      fill: {
        colors: [!dark ? "#202F9A" : "#73d6f1"],
        gradient: {
          gradientToColors: [!dark ? "#5F72FF" : "#73D6F1"],
        },
      },
    })
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [dark])

  return (
    <Box className={cx(classes.self)}>
      <Grid container spacing={3}>
        <Grid item sm={12} md={6} style={{ width: "100%" }}>
          <Box className={cx(classes.title)}>Volume</Box>
          <Box mt="20px" />
          <Box className={cx(classes.panel)}>
            <Box className={cx(classes.panelFilter)}>
              <Box className={cx(classes.panelFilterByType)}>
                <Box component="span">TOTAL</Box>
                <Box component="span">SWAP</Box>
                <Box component="span">ADD</Box>
                <Box component="span">WITHDRAW</Box>
              </Box>
              <Box className={cx(classes.panelFilterByDate)}>
                <Box component="span">WEEK</Box>
                <Box component="span">ALL</Box>
              </Box>
            </Box>
            <Box position="relative">
              <Chart
                options={volumeOptions}
                series={volumeSeries}
                type="bar"
                width="100%"
              />
              {!aggVolume && (
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
              )}
            </Box>
          </Box>
        </Grid>
        <Grid item sm={12} md={6} style={{ width: "100%" }}>
          <Box className={cx(classes.title)}>Liquidity</Box>
          <Box mt="20px" />
          <Box className={cx(classes.panel)}>
            <Box className={cx(classes.panelFilter)}>
              <Box className={cx(classes.panelFilterByType)}>
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
              <Chart
                options={liquidityOptions}
                series={liquiditySeries}
                type="area"
                width="100%"
              />
              {!aggLiquidity && (
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
              )}
            </Box>
          </Box>
        </Grid>
      </Grid>
    </Box>
  )
}

export default ChartSection
