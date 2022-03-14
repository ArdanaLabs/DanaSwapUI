import React, { useEffect, useMemo, useState } from "react"
import { Box, Grid, Typography, useMediaQuery } from "@material-ui/core"
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

import { ReactComponent as ArrowDown } from "assets/imgs/arrow-down.svg"
import { SwitchWithGlider } from "components"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    marginBottom: 50,
  },
  title: {
    color: palette.primary.main,
    fontWeight: 900,
    marginBottom: 30,
  },

  typo1: {
    fontSize: 16,
    color: palette.secondary.main,
  },

  typo2: {
    color: palette.primary.main,
  },

  totalTokensLocked: {
    display: "flex",
    flexDirection: "column",
    gridGap: "20px 0",
  },

  balancePanel: {
    background: `linear-gradient(126.33deg, ${palette.info.dark} 9.83%, rgba(37, 48, 130, 0) 96.44%)`,
    borderRadius: 5,
    padding: 25,
    display: "flex",
    flexDirection: "column",
    justifyContent: "center",
    gap: 20,
    height: 150,

    [`& .asset-logo`]: {
      width: 30,
      height: 30,
      marginRight: 10,
    },
  },

  tvlPanel: {
    position: "relative",
    padding: "15px 25px",
    borderTopRightRadius: 8,
    borderBottomRightRadius: 8,
    background: `linear-gradient(126.33deg, ${palette.info.dark} 9.83%, rgba(37, 48, 130, 0) 96.44%)`,
    display: "flex",
    flexDirection: "column",

    [`&::before`]: {
      content: '""',
      position: "absolute",
      left: 0,
      top: 0,
      width: 5,
      height: "100%",
      background: `linear-gradient(180deg, ${palette.secondary.main} 0%, ${palette.secondary.dark} 100%)`,
      borderRadius: "10px",
      fontFamily: "auto",
    },

    [`& svg`]: {
      marginRight: 3,
      [`&.inc`]: {
        transform: "rotateZ(180deg)",
        [`& > path`]: {
          stroke: palette.success.main,
        },
        [`& > rect`]: {
          fill: palette.success.main,
        },
      },
      [`&.dec`]: {
        [`& > path`]: {
          color: palette.error.main,
        },
        [`& > rect`]: {
          color: palette.error.main,
        },
      },
    },

    [`& .change`]: {
      lineHeight: "100%",
      [`&.inc`]: {
        color: palette.success.main,
      },
      [`&.dec`]: {
        color: palette.error.main,
      },
    },

    [`& span`]: {
      textTransform: "uppercase",
    },
  },

  liquidityPanel: {
    background: `linear-gradient(126.33deg, ${palette.info.dark} 9.83%, rgba(37, 48, 130, 0) 96.44%)`,
    height: "100%",
    maxHeight: "100%",
    borderRadius: 10,
    padding: 25,
    display: "flex",
    flexDirection: "column",
  },

  filterGroup: {
    display: "flex",
    justifyContent: "flex-end",
  },

  currentRatio: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "13px",
    lineHeight: "100%",
    color: palette.text.secondary,

    [`& > span`]: {
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

  const lockedTokens = useMemo(
    () => [
      {
        asset: Asset.iso.wrap("BTC"),
        amount: USD.iso.wrap(22_620_000),
      },
      {
        asset: Asset.iso.wrap("ARD"),
        amount: USD.iso.wrap(22_630_000),
      },
    ],
    []
  )
  const [activeChart, setActiveChart] = useState<ChartType>(ChartType.Volume)

  const poolVolumeChartOptions = useMemo(() => {
    let xCategories: any[] = []
    switch (poolVolume._tag) {
      case "Success":
        O.fold(
          (): void => {},
          (pvs: NEA.NonEmptyArray<VolumeChart.Item.Type>): void => {
            xCategories = extractDateAxis(pvs).map((d: Date) => printDate(d))
          }
        )(NEA.fromArray(poolVolume.success))
        break
      case "Pending":
      case "Failure":
        break
    }
    return xCategories
  }, [poolVolume])

  const poolLiquidityChartOptions = useMemo(() => {
    let xCategories: any[] = []
    switch (poolLiquidity._tag) {
      case "Success":
        O.fold(
          (): void => {},
          (pls: NEA.NonEmptyArray<LiquidityChart.Item.Type>) => {
            xCategories = extractDateAxis(pls).map((d: Date) => printDate(d))
          }
        )(NEA.fromArray(poolLiquidity.success))
        break
      case "Pending":
      case "Failure":
        break
    }
    return xCategories
  }, [poolLiquidity])

  const chartOptions: ApexOptions = useMemo(() => {
    let xCategories: any[] = []

    switch (activeChart) {
      case ChartType.Volume:
        xCategories = poolVolumeChartOptions
        break
      case ChartType.TVL:
        xCategories = poolVolumeChartOptions
        break
      case ChartType.Liquidity:
        xCategories = poolLiquidityChartOptions
        break
      default:
        break
    }

    return {
      chart: {
        id: `basic-bar-${ChartType.Volume}`,
        width: "100%",
        height: "100%",
        zoom: {
          enabled: false,
        },
        toolbar: {
          show: true,
          tools: {
            download: false,
          },
        },
        events: {
          mounted: (chart) => {
            chart.windowResizeHandler()
          },
        },
      },
      stroke: {
        width: 0,
        curve: "smooth",
      },
      xaxis: {
        categories: xCategories,
        labels: {
          show: true,
          style: {
            colors: palette.secondary.main,
            fontSize: !mobile ? "13px" : "8px",
            fontFamily: "Museo Sans",
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
            fontFamily: "Museo Sans",
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
  }, [
    palette,
    mobile,
    activeChart,
    poolVolumeChartOptions,
    poolLiquidityChartOptions,
  ])

  const poolVolumeChartSeries = useMemo(() => {
    let series: any[] = []
    switch (poolVolume._tag) {
      case "Success":
        O.fold(
          (): void => {},
          (pvs: NEA.NonEmptyArray<VolumeChart.Item.Type>): void => {
            series = pvs.map(([, v]: VolumeChart.Item.Type): number =>
              USD.iso.unwrap(Volume.iso.unwrap(v.total))
            )
          }
        )(NEA.fromArray(poolVolume.success))
        break
      case "Pending":
      case "Failure":
        break
    }
    return series
  }, [poolVolume])

  const poolLiquidityChartSeries: ApexAxisChartSeries = useMemo(() => {
    let series: any[] = []
    switch (poolLiquidity._tag) {
      case "Success":
        O.fold(
          (): void => {},
          (pls: NEA.NonEmptyArray<LiquidityChart.Item.Type>) => {
            series = pls.map(
              ([, l]: LiquidityChart.Item.Type): USD.Type =>
                LiquidityTokenPrice.iso.unwrap(l)
            )
          }
        )(NEA.fromArray(poolLiquidity.success))
        break
      case "Pending":
      case "Failure":
        break
    }
    return series
  }, [poolLiquidity])

  const chartSeries: ApexAxisChartSeries = useMemo(() => {
    switch (activeChart) {
      case ChartType.Volume:
        return [{ name: "Volume", data: poolVolumeChartSeries }]
      case ChartType.TVL:
        return [{ name: "TVL", data: poolVolumeChartSeries }]
      case ChartType.Liquidity:
        return [{ name: "Liquidity", data: poolLiquidityChartSeries }]
      default:
        return []
    }
  }, [activeChart, poolVolumeChartSeries, poolLiquidityChartSeries])

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

  function renderTotalTokensLocked(): JSX.Element {
    return (
      <Box className={classes.totalTokensLocked}>
        <Box className={classes.balancePanel}>
          {lockedTokens.map(
            (token: { amount: USD.Type; asset: Asset.Type }) => {
              const assetStr: string = Asset.iso.unwrap(token.asset)
              let icon
              try {
                icon = require(`assets/coins/${assetStr}.png`).default
              } catch (e) {
                icon = require(`assets/coins/BTC.png`).default
              }
              return (
                <Box
                  key={assetStr}
                  display="flex"
                  justifyContent={"space-between"}
                  alignItems={"center"}
                >
                  <Box display={"flex"} alignItems="center">
                    <img src={icon} alt={assetStr} className="asset-logo" />
                    <Typography
                      variant="h4"
                      component="span"
                      className={classes.typo1}
                    >
                      {assetStr}
                    </Typography>
                  </Box>
                  <Typography
                    variant="h4"
                    component="span"
                    className={classes.typo2}
                  >
                    {printCurrencyUSD(token.amount, {
                      minimumFractionDigits: 4,
                    })}
                  </Typography>
                </Box>
              )
            }
          )}
        </Box>
        <Box className={classes.tvlPanel}>
          <Typography variant="h4" component="span" className={classes.typo1}>
            TVL
          </Typography>
          <Box display={"flex"} alignItems="center">
            <Typography variant="h4" component="span" className={classes.typo2}>
              {printCurrencyUSD(USD.iso.wrap(242_900_000))}
            </Typography>
            <Box display={"flex"} alignItems="baseline" ml={3}>
              <ArrowDown className={cx({ dec: true })} />
              <Typography
                variant="h6"
                component="span"
                className={cx("change", { dec: true })}
              >
                2.05%
              </Typography>
            </Box>
          </Box>
        </Box>
        <Box className={classes.tvlPanel}>
          <Typography variant="h4" component="span" className={classes.typo1}>
            TVL
          </Typography>
          <Box display={"flex"} alignItems="center">
            <Typography variant="h4" component="span" className={classes.typo2}>
              {printCurrencyUSD(USD.iso.wrap(83_900_000))}
            </Typography>
            <Box display={"flex"} alignItems="baseline" ml={3}>
              <ArrowDown className={cx({ inc: true })} />
              <Typography
                variant="h6"
                component="span"
                className={cx("change", { inc: true })}
              >
                36.12%
              </Typography>
            </Box>
          </Box>
        </Box>
        <Box className={classes.tvlPanel}>
          <Typography variant="h4" component="span" className={classes.typo1}>
            24h Fees
          </Typography>
          <Box display={"flex"} alignItems="center">
            <Typography variant="h4" component="span" className={classes.typo2}>
              {printCurrencyUSD(USD.iso.wrap(264_890))}
            </Typography>
          </Box>
        </Box>
      </Box>
    )
  }

  function renderLiquidityChart(): JSX.Element {
    return (
      <Box className={classes.liquidityPanel}>
        <Box className={classes.filterGroup}>
          <SwitchWithGlider
            elements={allChartTypes.map(chartTypeLabel)}
            activeIndex={activeChart}
            handleSwitch={(index: ChartType) => setActiveChart(index)}
          />
        </Box>
        <Box height={"100%"}>
          <Chart
            options={chartOptions}
            series={chartSeries}
            type={activeChart === 1 ? "area" : "bar"}
            height={"100%"}
            width={"100%"}
          />
        </Box>
      </Box>
    )
  }

  return (
    <Box className={cx(classes.root)}>
      <Grid container spacing={3} alignItems={"stretch"}>
        <Grid item xs={12} sm={4}>
          <Box display={"flex"} flexDirection="column" height={"100%"}>
            <Typography
              variant="h1"
              component="h1"
              className={cx(classes.title)}
            >
              Total Tokens Locked
            </Typography>
            {renderTotalTokensLocked()}
          </Box>
        </Grid>
        <Grid item xs={12} sm={8}>
          <Box display={"flex"} flexDirection="column" height={"100%"}>
            <Typography
              variant="h1"
              component="h1"
              className={cx(classes.title)}
            >
              Liquidity
            </Typography>
            {renderLiquidityChart()}
          </Box>
        </Grid>
      </Grid>
    </Box>
  )
}

export default StatsSection
