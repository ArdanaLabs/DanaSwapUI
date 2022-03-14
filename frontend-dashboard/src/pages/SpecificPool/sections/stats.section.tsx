import React, { useEffect, useState } from "react"
import { Box, Grid, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import Chart from "react-apexcharts"
import { ApexOptions } from "apexcharts"

import * as NEA from "fp-ts/NonEmptyArray"
import * as O from "fp-ts/Option"

import * as PoolStats from "Data/Stats/PoolStats"
import * as PoolSetName from "Data/Pool/PoolSetName"
import * as LiquidityTokenPrice from "Data/LiquidityTokenPrice"
import * as Volume from "Data/Volume"
import * as Asset from "Data/Asset"
import { USD } from "Data/Unit"
import { LiquidityChart, VolumeChart } from "Data/Chart"
import { Granularity } from "Data/Chart/Granularity"
import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"
import { extractDateAxis, printCurrencyUSD, printDate } from "hooks"
import { usePoolVolume, usePoolLiquidity } from "state/chart/hooks"

import { SwitchWithGlider } from "components"

const useStyles = makeStyles(({ palette }) => ({
  root: {},
  panelbg: {
    background: palette.background.paper,
    padding: "20px",
    borderRadius: "10px",
    boxShadow: "2px 2px 10px rgba(0, 0, 0, 0.1)",
    height: "100%",
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "18px",
    lineHeight: "110%",
    color: palette.primary.main,
  },

  token: {
    "display": "flex",
    "justifyContent": "space-between",
    "alignItems": "center",
    "padding": "10px 0px",

    "fontFamily": "Museo Sans",
    "fontStyle": "normal",
    "fontWeight": 900,
    "lineHeight": "115%",
    "color": palette.secondary.main,

    "& > img": {
      width: "35px",
      height: "35px",
    },

    "& > span:first-of-type": {
      paddingLeft: "10px",
      fontSize: "12px",
      flexGrow: 5,
    },
    "& > span:last-of-type": {
      fontSize: "11px",
    },
  },

  statsBoxBg: {
    background: palette.type === "light" ? "#F5F5F5" : "#25308280",
    boxShadow: "0px 4px 4px rgba(0, 0, 0, 0.25)",
    borderRadius: "10px",
  },
  statsBox: {
    "position": "relative",
    "padding": "10px 30px",
    "margin": "20px 0px",

    "display": "flex",
    "flexDirection": "column",
    "fontFamily": "Museo Sans",
    "fontStyle": "normal",
    "lineHeight": "100%",

    "& > span:nth-of-type(1)": {
      color: palette.text.hint,
      fontSize: "11px",
      fontWeight: 300,
    },
    "& > span:nth-of-type(2)": {
      color: palette.secondary.main,
      fontSize: "17px",
      margin: "10px 0px",
      fontWeight: 500,
    },
    "& > span:nth-of-type(3)": {
      fontWeight: 300,
      fontSize: "14px",
    },
  },
  leftBorder: {
    position: "absolute",
    left: 0,
    top: 0,
    bottom: 0,
    height: "100%",
    background: palette.info.dark,
    borderRadius: "10px",
    fontFamily: "auto",
  },

  filterNormal: {
    border: "1px solid #FFFFFF",
    boxSizing: "border-box",
    borderRadius: "20px",
    padding: "10px 20px",
    background: palette.type === "light" ? "#A5A5A5" : "#010730",
    textTransform: "uppercase",

    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: "bold",
    fontSize: "9px",
    lineHeight: "100%",
    textAlign: "center",
    color: palette.common.white,
  },

  filterActive: {
    border: "unset",
    background:
      palette.type === "light"
        ? "linear-gradient(89.62deg, #000A4F 0.3%, #3C4DC5 99.64%)"
        : "linear-gradient(90deg, #5F72FF 0%, rgba(115, 214, 241, 0) 100%)",
  },

  currentRatio: {
    "fontFamily": "Museo Sans",
    "fontStyle": "normal",
    "fontWeight": 500,
    "fontSize": "13px",
    "lineHeight": "100%",
    "color": palette.text.secondary,

    "& > span": {
      fontWeight: 700,
      fontSize: "11px",
    },
  },
}))

enum ChartType {
  Volume = 0,
  TVL = 1,
  Liquidity = 2,
}

const allChartTypes: ChartType[] = [
  ChartType.Volume,
  ChartType.TVL,
  ChartType.Liquidity,
]

function chartTypeLabel(ct: ChartType): string {
  switch (ct) {
    case ChartType.Volume:
      return "Volume"
    case ChartType.TVL:
      return "TVL"
    case ChartType.Liquidity:
      return "Liquidity"
  }
}

export type Props = {
  poolStats: PoolStats.Type
  poolSet: PoolSetName.Type
}

const StatsSection: React.FC<Props> = ({ poolSet, poolStats }: Props) => {
  const { palette, breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const isDarkTheme: boolean = Theme.Eq.equals(userTheme, Theme.Theme.Dark)
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark: isDarkTheme, mobile })

  const { poolVolume, fetchPoolVolume } = usePoolVolume()
  const { poolLiquidity, fetchPoolLiquidity } = usePoolLiquidity()

  let options: ApexOptions = {
    chart: {
      id: "chart-trade-volume",
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
      categories: [],
      labels: {
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
      data: [10],
    },
  ]

  const lockedTokenList = [
    {
      asset: Asset.iso.wrap("BTC"),
      amount: USD.iso.wrap(22_620_000),
    },
    {
      asset: Asset.iso.wrap("ARD"),
      amount: USD.iso.wrap(22_630_000),
    },
  ]

  const [chartOptions, setChartOptions] = useState<ApexOptions>(options)
  const [chartSeries, setChartSeries] = useState<any[]>(series)

  const [activeChart, setActiveChart] = useState<ChartType>(ChartType.Volume)

  useEffect(() => {
    fetchPoolVolume(
      poolSet,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )
    fetchPoolLiquidity(
      poolSet,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  useEffect(() => {
    switch (poolVolume._tag) {
      case "Success":
        O.fold(
          (): void => {},
          (pvs: NEA.NonEmptyArray<VolumeChart.Item.Type>): void => {
            setChartOptions({
              ...chartOptions,
              xaxis: {
                categories: extractDateAxis(pvs).map((d: Date) => printDate(d)),
                labels: {
                  style: {
                    colors: palette.text.hint,
                  },
                },
                axisTicks: {
                  show: false,
                },
                axisBorder: {
                  show: false,
                },
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
            })
            setChartSeries([
              {
                name: "Volume",
                data: pvs.map(([, v]: VolumeChart.Item.Type): number =>
                  USD.iso.unwrap(Volume.iso.unwrap(v.total))
                ),
              },
            ])
          }
        )(NEA.fromArray(poolVolume.success))
        break
      case "Pending":
        break
      case "Failure":
        break
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [palette, poolVolume])

  const handleSwitch = (index: ChartType) => {
    setActiveChart(index)
    switch (index) {
      case ChartType.Volume:
        switch (poolVolume._tag) {
          case "Success":
            O.fold(
              (): void => {},
              (pvs: NEA.NonEmptyArray<VolumeChart.Item.Type>): void => {
                setChartOptions({
                  ...chartOptions,
                  xaxis: {
                    categories: extractDateAxis(pvs).map((d: Date) =>
                      printDate(d)
                    ),
                    labels: {
                      style: {
                        colors: palette.text.hint,
                      },
                    },
                    axisTicks: {
                      show: false,
                    },
                    axisBorder: {
                      show: false,
                    },
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
                })
                setChartSeries([
                  {
                    name: "Volume",
                    data: pvs.map(([, v]: VolumeChart.Item.Type): number =>
                      USD.iso.unwrap(Volume.iso.unwrap(v.total))
                    ),
                  },
                ])
              }
            )(NEA.fromArray(poolVolume.success))
            break
          case "Pending":
            break
          case "Failure":
            break
        }
        break
      case ChartType.TVL:
      case ChartType.Liquidity:
        switch (poolLiquidity._tag) {
          case "Success":
            O.fold(
              (): void => {},
              (pls: NEA.NonEmptyArray<LiquidityChart.Item.Type>) => {
                setChartOptions({
                  ...chartOptions,
                  xaxis: {
                    categories: extractDateAxis(pls).map((d: Date) =>
                      printDate(d)
                    ),
                    labels: {
                      style: {
                        colors: palette.text.hint,
                      },
                      formatter: (usd: any): string => printCurrencyUSD(usd),
                    },
                    axisTicks: {
                      show: false,
                    },
                    axisBorder: {
                      show: false,
                    },
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
                })
                setChartSeries([
                  {
                    name: "Liquidity",
                    data: pls.map(
                      ([, l]: LiquidityChart.Item.Type): USD.Type =>
                        LiquidityTokenPrice.iso.unwrap(l)
                    ),
                  },
                ])
              }
            )(NEA.fromArray(poolLiquidity.success))
            break
          case "Pending":
            break
          case "Failure":
            break
        }
        break
    }
  }

  return (
    <Box className={cx(classes.root)}>
      <Grid container spacing={3}>
        <Grid item xs={12} sm={4}>
          <Box className={cx(classes.title)}>Total Tokens Locked</Box>
        </Grid>
        <Grid item xs={12} sm={8}>
          <Box className={cx(classes.title)}>Trade Volume Graph</Box>
        </Grid>
      </Grid>
      <Grid container spacing={3}>
        <Grid item xs={12} sm={4}>
          <Box className={cx(classes.panelbg)}>
            <Box
              className={cx(classes.statsBoxBg)}
              paddingX="25px"
              paddingY="15px"
            >
              {lockedTokenList.map(
                (token: { asset: Asset.Type; amount: USD.Type }, i: number) => {
                  // TODO: make this an SVG sprite rather than PNG
                  const assetStr: string = Asset.iso.unwrap(token.asset)
                  const icon = require(`assets/coins/${assetStr}.png`).default
                  return (
                    <Box className={cx(classes.token)} key={i}>
                      <img src={icon} alt="" />
                      <span>{assetStr}</span>
                      <span>
                        {printCurrencyUSD(token.amount, {
                          minimumFractionDigits: 4,
                        })}
                      </span>
                    </Box>
                  )
                }
              )}
            </Box>
            <Box className={cx(classes.statsBox, classes.statsBoxBg)}>
              {/* TODO: don’t use div/Box for spacing */}
              <Box className={cx(classes.leftBorder)}>&nbsp;&nbsp;</Box>
              {/* TODO: text-transform: uppercase */}
              {/* TODO: what is the abbreviation?? */}
              <span>
                <abbr title="TODO">TVL</abbr>
              </span>
              <span>{printCurrencyUSD(USD.iso.wrap(242_900_000))}</span>
              <Box style={{ color: "red" }}>
                <i className="fal fa-long-arrow-down"></i>&nbsp;
                <span>2.05%</span>
              </Box>
            </Box>

            <Box className={cx(classes.statsBox, classes.statsBoxBg)}>
              <Box className={cx(classes.leftBorder)}>&nbsp;&nbsp;</Box>
              {/* TODO: text-transform: uppercase */}
              <span>VOLUME 24H</span>
              <span>
                {printCurrencyUSD(
                  O.getOrElse(() => USD.iso.wrap(0))(
                    poolStats.recentDailyVolumeUSD.trade
                  ),
                  { minimumFractionDigits: 2 }
                )}
              </span>
              {/* TODO: use class, use function */}
              <Box style={{ color: "green" }}>
                <i className="fal fa-long-arrow-up"></i>&nbsp;
                <span>36.12%</span>
              </Box>
            </Box>

            <Box className={cx(classes.statsBox, classes.statsBoxBg)}>
              {/* TODO: don’t use div/Box for spacing */}
              <Box className={cx(classes.leftBorder)}>&nbsp;&nbsp;</Box>
              {/* TODO: text-transform: uppercase */}
              <span>24H FEES</span>
              <span>
                {printCurrencyUSD(
                  O.getOrElse(() => USD.iso.wrap(0))(
                    poolStats.dailyFeeVolumeUSD
                  ),
                  { minimumFractionDigits: 2 }
                )}
              </span>
              {/* TODO: use class, use function */}
              <span style={{ color: "red" }}>&nbsp;</span>
            </Box>
          </Box>
        </Grid>
        <Grid item xs={12} sm={8}>
          <Box className={cx(classes.panelbg)}>
            <Box textAlign="right">
              <SwitchWithGlider
                elements={allChartTypes.map(chartTypeLabel)}
                normalClass={classes.filterNormal}
                activeClass={classes.filterActive}
                activeIndex={activeChart}
                handleSwitch={handleSwitch}
              />
            </Box>
            {activeChart !== 1 && (
              <Chart
                options={chartOptions}
                series={chartSeries}
                type="bar"
                width="100%"
              />
            )}
            {activeChart === 1 && (
              <Chart
                options={chartOptions}
                series={chartSeries}
                type="area"
                width="100%"
              />
            )}
            {activeChart === 2 && (
              <Box className={cx(classes.currentRatio)}>
                {/* TODO: text-transform: uppercase */}
                {/* TODO: remove the breaks for proper padding/gaps */}
                CURRENT PRICE
                <br />
                <br />
                <span>1 USDC = 0.0004 ETH</span>
                <br />
                <span>1 ETH = 2,359.0502 USDC</span>
              </Box>
            )}
          </Box>
        </Grid>
      </Grid>
    </Box>
  )
}

export default StatsSection
