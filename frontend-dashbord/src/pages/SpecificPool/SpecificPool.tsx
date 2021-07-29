import React, { useEffect, useState } from 'react'
import { Box, Fade, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import { useIsDarkMode } from 'state/user/hooks'
import cx from 'classnames'
import Button from 'components/Button/Button'
import { StatsSection, ChartSection, TransactionsSection } from './sections'
import { useHistory, useLocation } from 'react-router-dom'
import { usePoolStats } from 'state/home/hooks'

const FILTER_SWAP = 0
const FILTER_DEPOSIT = 1
const FILTER_WITHDRAW = 2

const useStyles = makeStyles(({ palette, breakpoints }) => ({
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

  filterType: {
    background: palette.secondary.dark,
    padding: '10px 30px',
    fontSize: '11px',
    lineHeight: '100%',
    marginLeft: '10px',

    [breakpoints.down('xs')]: {
      width: 'auto'
    }
  },

  active: {
    background: palette.primary.light
  }
}))

const SpecificPool: React.FC = () => {
  const { breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const dark = useIsDarkMode()
  const classes = useStyles({ dark, mobile })
  const location = useLocation()
  const history = useHistory()
  const poolStats = usePoolStats()

  const [filter, setFilter] = useState({
    text: '',
    type: FILTER_SWAP
  })

  const onFilterChange = (event: any) => {
    setFilter({ ...filter, ...event })
  }

  useEffect(() => {
    if (!location.state) {
      history.goBack()
      return
    }
    const { poolName }: any = location.state
    if (!poolName || !poolStats || !poolStats[poolName]) {
      history.goBack()
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [poolStats, location.state])

  return (
    <Fade in={true}>
      <Box>
        <Box textAlign='right'>
          <Button
            variant='contained'
            onClick={() => {
              onFilterChange({ type: FILTER_SWAP })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_SWAP
            })}
          >
            SWAP
          </Button>
          <Button
            variant='contained'
            onClick={() => {
              onFilterChange({ type: FILTER_DEPOSIT })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_DEPOSIT
            })}
          >
            DEPOSIT
          </Button>
          <Button
            variant='contained'
            onClick={() => {
              onFilterChange({ type: FILTER_WITHDRAW })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_WITHDRAW
            })}
          >
            WITHDRAW
          </Button>
        </Box>

        <Box mt={'30px'} />

        <StatsSection />

        <Box mt={'30px'} />

        <ChartSection />

        <Box mt={'50px'} />

        <TransactionsSection />
      </Box>
    </Fade>
  )
}

export default SpecificPool
