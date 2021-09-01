import React from 'react'
import { Box, Container, Grid, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'

import { useIsDarkMode } from 'state/user/hooks'

import BACKGROUND_COIN from 'assets/image/BACKGROUND-COIN.png'
import BACKGROUND_WAVE from 'assets/image/BACKGROUND-WAVE.png'
import COIN_CARDANO from 'assets/image/COIN1.png'

import COIN_DANA from 'assets/image/COIN-DANA-3D.png'
import COIN_ETH from 'assets/image/COIN-ETH-3D.png'
import COIN_HUOBI from 'assets/image/COIN-HUOBI-3D.png'

import { TokenCard } from 'components'

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    paddingBottom: '500px',
    background: `url(${BACKGROUND_COIN}) right top no-repeat, url(${BACKGROUND_WAVE}) left top no-repeat`,
    backgroundSize: '500px, contain'
  },

  description: {
    '& > div:first-child': {
      color: palette.primary.main,
      fontFamily: 'Brandon Grotesque',
      fontWeight: 700,
      fontSize: '50px',
      paddingBottom: '20px'
    },
    '& > div:last-child': {
      color: palette.primary.main,
      fontFamily: 'Museo Sans',
      fontWeight: 100,
      fontSize: '20px'
    }
  },

  coins: {
    '& > img': {
      marginRight: '20px',
      opacity: '0.5'
    }
  },

  connectWallet: {
    background: palette.info.light,
    padding: '5px 15px',
    fontFamily: 'Brandon Grotesque',
    fontSize: '14px',
    color: palette.common.white,
    borderRadius: '100px',
    display: 'inline-block',
    fontWeight: 700
  }
}))

const AdSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box mt='100px' />

        <Box className={cx(classes.description)}>
          <Box>
            Collateral assets can be leveraged to
            <br /> mint Ardana Stablecoins.
          </Box>
          <Box>
            Open a Ardana Stablecoin Vault, deposit your
            <br /> collateral, and generate dUSD against it.
          </Box>
        </Box>

        <Box mt='50px' />

        <Box className={cx(classes.coins)}>
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
        </Box>

        <Box mt='40px' />

        <Box className={cx(classes.connectWallet)}>CONNECT A WALLET</Box>

        <Box mt='200px' />

        <Grid container spacing={4}>
          <Grid item xs={12} sm={6} md={4}>
            <TokenCard
              name='LINK-A'
              label='NEW'
              stabilityFee={3}
              ratio={165}
              image={COIN_DANA}
              background={
                'linear-gradient(180deg, #3142A3 0%, rgba(49, 66, 163, 0) 118.48%)'
              }
            />
          </Grid>
          <Grid item xs={12} sm={6} md={4}>
            <TokenCard
              name='UNI-A'
              label='MOST POPULAR'
              stabilityFee={2}
              ratio={145}
              image={COIN_ETH}
              background={
                'linear-gradient(180.2deg, #627EFF 0.17%, rgba(77, 97, 210, 0) 116.51%)'
              }
            />
          </Grid>
          <Grid item xs={12} sm={6} md={4}>
            <TokenCard
              name='GUSD-A'
              label='CHEAPEST'
              stabilityFee={0}
              ratio={101}
              image={COIN_HUOBI}
              background={
                'linear-gradient(180deg, #71CEF3 0%, rgba(113, 206, 243, 0) 110%)'
              }
            />
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default AdSection
