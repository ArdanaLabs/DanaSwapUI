import React, { useState } from 'react'
import {
  Box,
  useMediaQuery,
  TableContainer,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
  TableFooter
} from '@material-ui/core'
import { makeStyles, useTheme, withStyles } from '@material-ui/core/styles'
import cx from 'classnames'
import { useIsDarkMode } from 'state/user/hooks'
import { SearchInput } from 'components/Input'
import { Button } from 'components/Button'
import { usePoolStats } from 'state/home/hooks'
import { keys } from 'lodash'
import { useHistory } from 'react-router-dom'
import { nFormatter } from 'hooks'

const FILTER_STABLECOINS = 0
const FILTER_DIGITALASSESTS = 1

const StyledTableCell = withStyles(({ palette }) => ({
  root: {
    borderBottom: '1px solid #E5E5E5'
  },
  head: {
    fontFamily: 'Museo Sans',
    fontStyle: 'normal',
    fontWeight: 500,
    fontSize: '16px',
    lineHeight: '300%',
    textAlign: 'center',
    cursor: 'pointer',
    color: palette.secondary.main
  },
  body: {
    fontFamily: 'Museo Sans',
    fontStyle: 'normal',
    fontWeight: 'bold',
    fontSize: '16px',
    lineHeight: '115%',
    textAlign: 'center',
    color: palette.secondary.main,
    cursor: 'pointer'
  },
  footer: {
    width: '100%',
    height: 30,
    textAlign: 'center',

    '& span': {
      fontFamily: 'Museo Sans',
      fontStyle: 'normal',
      fontWeight: 'bold',
      fontSize: '16px',
      lineHeight: '115%',
      cursor: 'pointer',
      color: palette.secondary.main,

      '&:hover': {
        color: 'gray'
      }
    }
  }
}))(TableCell)

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  panel: {
    background: palette.background.paper,
    borderRadius: '10px',
    padding: '12px'
  },
  filter: {
    display: 'flex',
    justifyContent: 'space-between',
    alignItems: 'center',
    margin: '20px 0',

    [breakpoints.down('xs')]: {
      flexDirection: 'column',
      '& > div': {
        width: '100%'
      }
    }
  },

  filterText: {
    background:
      palette.type === 'light'
        ? '#E7E7E7'
        : 'linear-gradient(90deg, #3142A3 0%, #243183 100%)',
    boxShadow:
      palette.type === 'light'
        ? 'unset'
        : '2px 2px 10px rgba(0, 0, 0, 0.1), inset 2px 2px 10px rgba(0, 0, 0, 0.1)',
    fontSize: '11px',
    fontWeight: 600,
    lineHeight: '100%',
    width: '500px',
    padding: '15px 30px',
    borderRadius: '20px',
    color: palette.secondary.main,

    '& ::placeholder': {
      color: palette.secondary.main
    },

    [breakpoints.down('xs')]: {
      flexDirection: 'column',
      width: '100%'
    }
  },
  filterType: {
    background: palette.secondary.dark,
    padding: '15px',
    fontSize: '11px',
    lineHeight: '100%',
    marginLeft: '10px',
    width: '150px',

    [breakpoints.down('xs')]: {
      width: 'auto'
    }
  },

  active: {
    background: palette.primary.light
  }
}))

export interface PoolsPanelProps {
  overView?: boolean
}

const PoolsPanel: React.FC<PoolsPanelProps> = ({ overView = false }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })
  const history = useHistory()

  const columns = [
    'POOL',
    'Liquidity',
    'Base APY',
    'Rewards APY',
    'VOLUME',
    'APY'
  ]

  const poolStats = usePoolStats()
  const poolNames = keys(poolStats)
  // const poolReserves = poolInfos.map((val: any) => {
  //   let reserves = keys(val.reserves)
  //   return reserves.join(' + ')
  // })
  // const poolRewardAPYs = poolInfos.map((val: any) => {
  //   const { reserves, totalAPYPercent, recentAnnualAPYPercent } = val
  //   const reserveNames = keys(reserves)
  //   const reserveVals = values(reserves)
  //   const formattedReserves: string[] = reserveNames.map(
  //     (reserveName: string, i: number) => {
  //       return reserveVals[i] + '% ' + reserveName
  //     }
  //   )
  //   const changedPercent = totalAPYPercent - recentAnnualAPYPercent

  //   return (
  //     (changedPercent < 0 ? '' : '+') +
  //     changedPercent +
  //     '%' +
  //     ' -> ' +
  //     formattedReserves.join(' + ')
  //   )
  // })

  const [filter, setFilter] = useState({
    text: '',
    type: FILTER_STABLECOINS
  })

  const onFilterChange = (event: any) => {
    setFilter({ ...filter, ...event })
  }

  const handleRowClick = (event: any, poolName: string) => {
    history.push({
      pathname: '/spec',
      state: {
        poolName: poolName
      }
    })
  }

  return (
    <Box>
      {!overView && (
        <Box className={cx(classes.filter)}>
          <SearchInput
            className={cx(classes.filterText)}
            value={filter.text}
            placeholder='Search pool by name, network or type...'
            onChange={(e: any) => {
              onFilterChange({ text: e.target.value })
            }}
          />

          <Box textAlign='center' mt={mobile ? '20px' : '0px'}>
            <Button
              variant='contained'
              onClick={() => {
                onFilterChange({ type: FILTER_STABLECOINS })
              }}
              className={cx(classes.filterType, {
                [classes.active]: filter.type === FILTER_STABLECOINS
              })}
            >
              STABLECOINS
            </Button>
            <Button
              variant='contained'
              onClick={() => {
                onFilterChange({ type: FILTER_DIGITALASSESTS })
              }}
              className={cx(classes.filterType, {
                [classes.active]: filter.type === FILTER_DIGITALASSESTS
              })}
            >
              DIGITAL ASSETS
            </Button>
          </Box>
        </Box>
      )}
      <TableContainer component={Box} className={cx(classes.panel)}>
        <Table aria-label='simple table'>
          <TableHead>
            <TableRow>
              {columns.map((column: any, i: any) => {
                return (
                  <StyledTableCell key={i}>
                    {column}
                    &nbsp;
                    <i className='fas fa-sort' />
                  </StyledTableCell>
                )
              })}
            </TableRow>
          </TableHead>
          <TableBody>
            {poolNames.map((poolName: any, i: any) => {
              // const icon = require(`assets/coins/${poolName}.png`).default
              const icon = require(`assets/coins/bBTC.png`).default
              return (
                <TableRow
                  key={i}
                  onClick={(e: any) => handleRowClick(e, poolName)}
                >
                  {/* POOL */}
                  <StyledTableCell component='th' scope='row'>
                    <Box display='flex'>
                      <Box>
                        <img
                          src={icon}
                          alt={poolName}
                          style={{ marginRight: '15px' }}
                        />
                      </Box>
                      <Box
                        display={'flex'}
                        flexDirection={'column'}
                        justifyContent={'center'}
                      >
                        {/* <Box textAlign='left'>{poolName}</Box>
                        <Box fontWeight={300}>{poolReserves[i]}</Box> */}
                        <Box textAlign='left'>sUSD</Box>
                        <Box fontWeight={300}>DAI + USDC + USDT + sUSD</Box>
                      </Box>
                    </Box>
                  </StyledTableCell>
                  {/* Liquidity */}
                  <StyledTableCell>
                    {nFormatter(poolStats && poolStats[poolName].navUSD)}
                  </StyledTableCell>
                  {/* Base APY */}
                  <StyledTableCell>
                    {/* {poolInfos[i].recentDailyAPYPercent}% */}
                    2.99%
                  </StyledTableCell>
                  {/* Rewards APY */}
                  <StyledTableCell>
                    {/* {poolRewardAPYs[i]} */}
                    {`+4.30% -> 10.76% DANA + 1.13% BTC`}
                  </StyledTableCell>
                  {/* VOLUME */}
                  <StyledTableCell>
                    {nFormatter(
                      poolStats &&
                        poolStats[poolName].recentDailyVolumeUSD.trade
                    )}
                  </StyledTableCell>
                  {/* APY */}
                  <StyledTableCell>29%</StyledTableCell>
                </TableRow>
              )
            })}
          </TableBody>
          {overView && (
            <TableFooter>
              <TableRow>
                <StyledTableCell colSpan={12}>
                  <Box component='span' onClick={() => history.push('/pools')}>
                    See All Pools
                  </Box>
                </StyledTableCell>
              </TableRow>
            </TableFooter>
          )}
        </Table>
      </TableContainer>
    </Box>
  )
}

export default PoolsPanel
