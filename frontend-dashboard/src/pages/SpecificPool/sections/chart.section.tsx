import React, { useEffect, useState } from "react"
import { Box, Grid, Typography, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import Chart from "react-apexcharts"
import { ApexOptions } from "apexcharts"

import * as O from "fp-ts/Option"

import * as AmplificationCoefficient from "Data/AmplificationCoefficient"
import * as Address from "Data/Address"
import * as APY from "Data/APY"
import * as Asset from "Data/Asset"
import * as Balances from "Data/Balance"
import * as TimeInterval from "Data/TimeInterval"
import * as FeeVolume from "Data/FeeVolume"
import * as TxCount from "Data/TxCount"
import { TxCountChart } from "Data/Chart"
import { Granularity } from "Data/Chart/Granularity"
import { Percent, USD } from "Data/Unit"
import * as PoolStats from "Data/Stats/PoolStats"
import * as PoolSetName from "Data/Pool/PoolSetName"

import { printCurrencyUSD, printPercentage } from "hooks"
import { usePoolAPY, usePoolFees, usePoolTxCount } from "state/chart/hooks"
import { useIsDarkMode } from "state/user/hooks"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    marginBottom: 50,
  },

  title: {
    color: palette.text.primary,
    marginBottom: 20,
  },

  panel: {
    background: `linear-gradient(126.33deg, ${palette.info.dark} 9.83%, rgba(37, 48, 130, 0) 96.44%)`,
    borderRadius: 10,
    padding: 20,
    height: "calc(100% - 70px)",
  },
}))

type Reserve = {
  asset: Asset.Type
  balances: Balances.Type
  ratios: { actual: Percent.Type; nominal: Percent.Type }
}

type CurrencySum = {
  label: string
  values: Balances.Type
}

export type Props = {
  poolSet: PoolSetName.Type
  poolStats: PoolStats.Type
}

export const ChartSection: React.FC<Props> = ({
  poolSet,
  poolStats,
}: Props): JSX.Element => {
  const { palette, breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const { poolAPY, fetchPoolAPY } = usePoolAPY()
  const { poolFees, fetchPoolFees } = usePoolFees()
  const { poolTxCount, fetchPoolTxCount } = usePoolTxCount()

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
      data: [10],
    },
  ]

  const [reserves, setReserves] = useState<Reserve[]>([])
  const [currencySum, setCurrencySum] = useState<CurrencySum>({
    label: "",
    values: {
      nominal: Balances.Nominal.iso.wrap(USD.iso.wrap(0)),
      actual: Balances.Actual.iso.wrap(USD.iso.wrap(0)),
    },
  })
  const [APYChartOptions, setAPYChartOptions] = useState<ApexOptions>(options)
  const [APYChartSeries, setAPYChartSeries] = useState<any[]>(series)
  const [FeesChartOptions, setFeesChartOptions] = useState<ApexOptions>(options)
  const [FeesChartSeries, setFeesChartSeries] = useState<any[]>(series)
  const [TxChartOptions, setTxChartOptions] = useState<ApexOptions>(options)
  const [TxChartSeries, setTxChartSeries] = useState<any[]>(series)

  useEffect(() => {
    fetchPoolAPY(
      poolSet,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()
    fetchPoolFees(
      poolSet,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()
    fetchPoolTxCount(
      poolSet,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()

    // Warning: mutable mess ahead
    // This this exists to be a bit more performant by unwraping less

    let reserves_: object[] = []
    let sum_: { label: string; values: { actual: number; nominal: number } } = {
      label: "",
      values: { actual: 0, nominal: 0 },
    }

    for (const [asset, balances] of poolStats.reserves.entries()) {
      reserves_.push({ asset, balances })
      const assetStr: string = Asset.iso.unwrap(asset)
      sum_.label = sum_.label === "" ? assetStr : `${sum_.label} + ${assetStr}`
      sum_.values.actual =
        sum_.values.actual +
        USD.iso.unwrap(Balances.Actual.iso.unwrap(balances.actual))
      sum_.values.nominal =
        sum_.values.nominal +
        USD.iso.unwrap(Balances.Nominal.iso.unwrap(balances.nominal))
    }

    // Add ratios
    const actualReserves: Reserve[] = reserves_.map((r: any): Reserve => {
      const actual: Percent.Type = Percent.iso.wrap(
        (100 * USD.iso.unwrap(Balances.Actual.iso.unwrap(r.balances.actual))) /
          sum_.values.actual
      )
      const nominal: Percent.Type = Percent.iso.wrap(
        (100 *
          USD.iso.unwrap(Balances.Nominal.iso.unwrap(r.balances.nominal))) /
          sum_.values.nominal
      )
      r.ratios = { actual, nominal }
      return r
    })

    setReserves(actualReserves)

    // wrapping up our values in newtypes
    const actualSum: CurrencySum = {
      label: sum_.label,
      values: {
        nominal: Balances.Nominal.iso.wrap(USD.iso.wrap(sum_.values.nominal)),
        actual: Balances.Actual.iso.wrap(USD.iso.wrap(sum_.values.actual)),
      },
    }

    setCurrencySum(actualSum)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  useEffect(() => {
    switch (poolAPY._tag) {
      case "Success":
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
            data: poolAPY.success.map(
              ([, p]: [TimeInterval.Type, APY.Type]): number =>
                APY.iso.unwrap(p)
            ),
          },
        ])
        break
      case "Pending":
        break
      case "Failure":
        break
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [poolAPY, palette])

  useEffect(() => {
    switch (poolFees._tag) {
      case "Success":
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
            data: poolFees.success.map(
              ([, f]: [TimeInterval.Type, FeeVolume.Type]): number =>
                Percent.iso.unwrap(FeeVolume.iso.unwrap(f))
            ),
          },
        ])
        break
      case "Pending":
        break
      case "Failure":
        break
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [poolFees, palette])

  useEffect(() => {
    switch (poolTxCount._tag) {
      case "Success":
        setTxChartOptions({
          ...TxChartOptions,
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
        setTxChartSeries([
          {
            name: "TxCount",
            data: poolTxCount.success.map(
              ([, t]: TxCountChart.Item.Type): number =>
                TxCount.iso.unwrap(t.total)
            ),
          },
        ])
        break
      case "Pending":
        break
      case "Failure":
        break
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [poolTxCount, palette])

  function renderVirtualPrice(
    subPoolAddress: Address.Type,
    virtualPrice: USD.Type
  ): JSX.Element {
    const subPoolAddressStr: string = Address.iso.unwrap(subPoolAddress)
    const key: string = subPoolAddressStr
    return (
      <React.Fragment key={key}>
        <dt>{subPoolAddressStr}</dt>
        <dd>{printCurrencyUSD(virtualPrice)}</dd>
      </React.Fragment>
    )
  }

  function renderVirtualPrices(
    virtualPrices: ReadonlyMap<Address.Type, USD.Type>
  ): JSX.Element {
    let elements: JSX.Element[] = []
    for (const [spa, vp] of virtualPrices.entries()) {
      elements.push(renderVirtualPrice(spa, vp))
    }
    return <dl>{elements}</dl>
  }

  function renderReserve(
    { asset, balances, ratios }: Reserve,
    _index: number
  ): JSX.Element {
    const assetStr: string = Asset.iso.unwrap(asset)
    return (
      <Box component="div" key={assetStr}>
        <dt>{assetStr}</dt>
        <dd>
          {printCurrencyUSD(Balances.Nominal.iso.unwrap(balances.nominal))} (
          {printPercentage(ratios.nominal)})
        </dd>
      </Box>
    )
  }

  return (
    <Box className={cx(classes.root)}>
      <Grid container spacing={3}>
        <Grid item sm={12} md={6} style={{ width: "100%", gap: "20px" }}>
          <Typography variant="h1" component="h1" className={classes.title}>
            APY Graph
          </Typography>
          <Box className={cx(classes.panel)}>
            <Chart
              options={APYChartOptions}
              series={APYChartSeries}
              type="area"
              width="100%"
            />
          </Box>
        </Grid>
        <Grid item sm={12} md={6}>
          <Typography variant="h1" component="h1" className={classes.title}>
            Historical Fee Data
          </Typography>
          <Box className={cx(classes.panel)}>
            <Chart
              options={FeesChartOptions}
              series={FeesChartSeries}
              type="area"
              width="100%"
            />
          </Box>
        </Grid>
        <Grid item sm={12} md={6}>
          <Typography variant="h1" component="h1" className={classes.title}>
            Currency Reserves
          </Typography>
          <Box
            component="dl"
            className={cx(classes.panel)}
            padding="35px !important"
          >
            {reserves.map(renderReserve)}

            <dt>{currencySum.label}</dt>
            <dd>
              {printCurrencyUSD(
                Balances.Nominal.iso.unwrap(currencySum.values.nominal)
              )}
            </dd>
            <dt>USD total (NAV)</dt>
            <dd>
              {printCurrencyUSD(
                O.getOrElse(() => USD.iso.wrap(0))(poolStats.navUSD)
              )}
            </dd>
            <dt>Fee</dt>
            <dd>
              {printPercentage(
                O.getOrElse(() => Percent.iso.wrap(0))(poolStats.feePercent)
              )}
            </dd>
            <dt>Admin fee</dt>
            <dd>
              {printPercentage(
                O.getOrElse(() => Percent.iso.wrap(0))(
                  poolStats.adminFeePercent
                )
              )}
            </dd>
            {O.fold(
              () => null,
              (vps: ReadonlyMap<Address.Type, USD.Type>) => (
                <React.Fragment>
                  <dt>Virtual price [?]</dt>
                  <dd>{renderVirtualPrices(vps)}</dd>
                </React.Fragment>
              )
            )(poolStats.virtualPriceUSD)}
            {O.fold(
              () => null,
              (a: AmplificationCoefficient.Type) => (
                <React.Fragment>
                  <dt>A</dt>
                  <dd>{AmplificationCoefficient.iso.unwrap(a)}</dd>
                </React.Fragment>
              )
            )(poolStats.amplificationCoefficient)}
          </Box>
        </Grid>
        <Grid item sm={12} md={6}>
          <Typography variant="h1" component="h1" className={classes.title}>
            <abbr title="Transaction">TX</abbr> Graph
          </Typography>
          <Box className={cx(classes.panel)}>
            <Chart
              options={TxChartOptions}
              series={TxChartSeries}
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
