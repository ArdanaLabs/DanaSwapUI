import React, { useEffect, useState } from 'react'
import { Box, Grid, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'
import Chart from 'react-apexcharts'

import { useIsDarkMode } from 'state/user/hooks'
import { ApexOptions } from 'apexcharts'
import { useLocation } from 'react-router-dom'
import { usePoolStats } from 'state/home/hooks'

const useStyles = makeStyles(({ palette }) => ({
  self: {
    background: 'unset'
  },

  title: {
    color: palette.text.primary,
    fontFamily: 'Brandon Grotesque',
    fontStyle: 'normal',
    fontWeight: 900,
    marginTop: '20px',
    fontSize: '18px',
    lineHeight: '110%'
  },

  panel: {
    background:
      palette.type === 'light'
        ? palette.common.white
        : palette.background.paper,
    borderRadius: '10px',
    padding: '20px',
    height: 'calc(100% - 70px)',

    fontFamily: 'Museo Sans',
    fontStyle: 'normal',
    fontSize: '11px',
    lineHeight: '115%',
    fontWeight: 100,
    color: palette.secondary.main
  }
}))

const ChartSection: React.FC = () => {
  const { palette, breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })
  const location = useLocation()
  const poolStats = usePoolStats()

  const options: ApexOptions = {
    chart: {
      id: 'basic-bar',
      zoom: {
        enabled: false
      },
      toolbar: {
        show: false
      }
    },
    stroke: {
      width: 0,
      curve: 'smooth'
    },
    xaxis: {
      categories: ['APR 20', 'MAY 15', 'JUN 02'],
      labels: {
        style: {
          colors: [palette.text.hint, palette.text.hint, palette.text.hint],
          fontSize: '11px',
          fontFamily: 'Museo Sans',
          fontWeight: 500
        }
      },
      tickPlacement: 'between'
    },
    yaxis: {
      labels: {
        show: true,
        align: 'left',
        style: {
          colors: [palette.secondary.main],
          fontFamily: 'Museo Sans',
          fontWeight: 'bold',
          fontSize: '16px',
          cssClass: 'apexcharts-yaxis-label'
        },
        formatter: (value: any) => {
          return '$' + value + (value ? ' M' : '')
        }
      }
    },
    fill: {
      type: 'gradient',
      colors: [!dark ? '#202F9A' : '#73d6f1'],
      gradient: {
        type: 'vertical', // The gradient in the horizontal direction
        gradientToColors: [!dark ? '#5F72FF' : '#73D6F1'], // The color at the end of the gradient
        opacityFrom: 1, // transparency
        opacityTo: 0.3,
        stops: [0, 1200]
      }
    },
    grid: {
      show: false
    },
    plotOptions: {
      bar: {
        borderRadius: 5
      }
    },
    dataLabels: {
      enabled: false
    },
    tooltip: {
      enabled: false
    }
  }

  const series = [
    {
      name: 'series-1',
      data: [30, 40, 45, 50, 49, 60, 70, 91, 30, 40, 45, 50, 49, 60, 70, 91]
    }
  ]

  const [poolInfo, setPoolInfo] = useState<any>(null)
  const [reserves, setReserves] = useState<any[]>([])
  const [currencySUM, setCurrencySUM] = useState({ label: '', value: 0 })

  useEffect(() => {
    const { poolName }: any = location.state
    poolStats && poolStats[poolName] && setPoolInfo(poolStats[poolName])
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [poolStats, location.state])

  useEffect(() => {
    if (poolInfo) {
      console.log('---------', poolInfo)

      const currencyNames: string[] = Object.keys(poolInfo.reserves)
      const currencyValues: number[] = Object.values(poolInfo.reserves)

      const _currencySUM: number = currencyValues.reduce(
        (accumulator: number, current: number) => accumulator + current
      )

      const parsedReserves: any[] = currencyNames.map(
        (currencyName: string, index: number) => ({
          name: currencyName,
          value: currencyValues[index],
          ratio: ((100 * currencyValues[index]) / _currencySUM).toFixed(2)
        })
      )
      setReserves(parsedReserves)
      setCurrencySUM({
        label: currencyNames.join(' + '),
        value: _currencySUM
      });
    }
  }, [poolInfo])

  return (
    <Box className={cx(classes.self)}>
      <Grid container spacing={3}>
        <Grid item sm={12} md={6} style={{ width: '100%' }}>
          <Box className={cx(classes.title)}>APY Graph</Box>
          <Box mt='20px' />
          <Box className={cx(classes.panel)}>
            <Chart options={options} series={series} type='area' width='100%' />
          </Box>
        </Grid>
        <Grid item sm={12} md={6} style={{ width: '100%' }}>
          <Box className={cx(classes.title)}>Historical Fee Data</Box>
          <Box mt='20px' />
          <Box className={cx(classes.panel)}>
            <Chart options={options} series={series} type='area' width='100%' />
          </Box>
        </Grid>
        <Grid item sm={12} md={6} style={{ width: '100%' }}>
          <Box className={cx(classes.title)}>Currency Reserves</Box>
          <Box mt='20px' />
          <Box className={cx(classes.panel)} padding='35px !important'>
            {/* <b>USDT:</b> 3,884.66445394 (40.77%)
            <br />
            <b>USDC:</b> 3,680.61262405 (38.63%)
            <br />
            <b>DAI:</b> 1,962.26649344 (20.60%)
            <br />
            <b>USDT+USDC+DAI:</b> 9,527.54357143
            <br /> */}
            {reserves &&
              reserves.map((currency: any, i: number) => (
                <Box component='p' key={i} margin={0}>
                  <b>{currency.name}:</b>&nbsp;{currency.value}&nbsp;({currency.ratio}%)
                </Box>
              ))}
            <b>{currencySUM.label}:</b>&nbsp;{currencySUM.value}
            <br/>
            <b>USD total (NAV):</b> ${(poolInfo && poolInfo.navUSD) ? poolInfo.navUSD.toLocaleString() : 0}
            <br />
            <br />
            <b>Fee:</b> {(poolInfo && poolInfo.feePercent) ? poolInfo.feePercent.toLocaleString() : 0}%
            <br />
            <b>Admin fee:</b> {(poolInfo && poolInfo.adminFeePercent) ? poolInfo.adminFeePercent.toLocaleString() : 0}%
            <br />
            <br />
            <b>Virtual price:</b> {(poolInfo && poolInfo.virtualPriceUSD) ? poolInfo.virtualPriceUSD.toLocaleString() : 0} [?]
            <br />
            <b>A:</b> {(poolInfo && poolInfo.amplificationCoefficient) ? poolInfo.amplificationCoefficient.toLocaleString() : 0}
          </Box>
        </Grid>
        <Grid item sm={12} md={6} style={{ width: '100%' }}>
          <Box className={cx(classes.title)}>TX Graph</Box>
          <Box mt='20px' />
          <Box className={cx(classes.panel)}>
            <Chart options={options} series={series} type='area' width='100%' />
          </Box>
        </Grid>
      </Grid>
    </Box>
  )
}

export default ChartSection
