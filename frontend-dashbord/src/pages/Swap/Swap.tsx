import React from 'react'
import { Box, Grid, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import Collapse from '@material-ui/core/Collapse'
import cx from 'classnames'

import { useIsDarkMode } from 'state/user/hooks'
import LandingImage from 'components/LandingImage'
import SwapForm from 'components/SwapForm'
import { Button, Radio, DropdownButton } from 'components/Button'
import { options, pools } from 'data';
import PoolsPanel from 'components/Table'

const useStyles = makeStyles(({ palette }) => ({
  homeContainer: {
    height: '100%'
  },
  swapPanel: {
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'flex-end',
    lineHeight: '35px',
    '& > span': {
      fontSize: '14px',
      color: palette.text.secondary,
      textAlign: 'right'
    }
  }
}))

const Swap: React.FC = () => {
  const { palette, breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })
  const [isOptionOpen, setIsOptionOpen] = React.useState(false)

  const onToggleOptions = () => {
    setIsOptionOpen(prev => !prev)
  }

  return (
    <Box style={mobile ? { backgroundColor: palette.background.paper } : {}}>
      <LandingImage url={'HOME > SWAP'} title={'Swap'} />
      <Box mt='12px'></Box>
      <SwapForm />
      <Box mt='4px'></Box>
      <Grid container spacing={mobile ? 1 : 2}>
        <Grid container item sm={6}>
          <Box
            position={'sticky'}
            width={'100%'}
            bgcolor={palette.secondary.main}
            pt={mobile ? '15px' : '37px'}
            pb={mobile ? '28px' : '37px'}
            pl={mobile ? '14px' : '37px'}
            pr={mobile ? '14px' : '37px'}
            borderRadius={'5px'}
          >
            <Box
              onClick={onToggleOptions}
              display={'flex'}
              style={{ cursor: 'pointer' }}
            >
              <Box mr={'7px'}>Advanced Options</Box>
              <DropdownButton isOpen={isOptionOpen} />
            </Box>
            <Collapse in={isOptionOpen}>
              <Grid
                container
                spacing={mobile ? 1 : 2}
                style={{ marginTop: '10px' }}
              >
                {options.map((option, i) => (
                  <Grid container item xs={4} key={i}>
                    <Radio option={option} value={option.data[0].value} />
                  </Grid>
                ))}
              </Grid>
            </Collapse>
          </Box>
        </Grid>
        <Grid container item sm={6}>
          <Box
            position={'sticky'}
            width={'100%'}
            pt={mobile ? '15px' : '37px'}
            pb={mobile ? '28px' : '37px'}
            pl={mobile ? '14px' : '37px'}
            pr={mobile ? '14px' : '37px'}
            className={cx(classes.swapPanel)}
          >
            <span>Exchange rate DAI/USDC (including fees): 1.0006</span>
            <span style={{ marginBottom: '15px' }}>
              Trade routed through: sUSD
            </span>
            <Button variant='contained'>Swap</Button>
          </Box>
        </Grid>
      </Grid>
      <Box mt='20px'></Box>
      <PoolsPanel data={pools} overView={true}></PoolsPanel>
    </Box>
  )
}

export default Swap
