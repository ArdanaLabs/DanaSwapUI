import React from 'react'
import { Box, Container, Grid, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'

import { useIsDarkMode } from 'state/user/hooks'

import BACKGROUND_COIN from 'assets/image/BACKGROUND-COIN.png'
import BACKGROUND_COIN_MOBILE from 'assets/image/BACKGROUND-COIN-MOBILE.png'
import BACKGROUND_WAVE from 'assets/image/BACKGROUND-WAVE.png'
import COIN_CARDANO from 'assets/image/COIN1.png'

import COIN_DANA from 'assets/image/COIN-DANA-3D.png'
import COIN_ETH from 'assets/image/COIN-ETH-3D.png'
import COIN_HUOBI from 'assets/image/COIN-HUOBI-3D.png'

import { ThemeSwitch, TokenCard } from 'components'

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: `url(${BACKGROUND_COIN}) right top no-repeat, url(${BACKGROUND_WAVE}) left top no-repeat`,
    backgroundSize: '500px, contain',

    [breakpoints.down('sm')]: {
      background: `url(${BACKGROUND_COIN_MOBILE}) no-repeat, url(${BACKGROUND_WAVE}) left top 350px no-repeat`,
      backgroundSize: 'contain',
      textAlign: 'center'
    }
  },

  description: {
    '& > div:first-child': {
      color: palette.primary.main,
      fontFamily: 'Brandon Grotesque',
      fontWeight: 700,
      fontSize: '50px',
      paddingBottom: '20px',
      lineHeight: '110%',

      [breakpoints.down('sm')]: {
        fontSize: '30px'
      }
    },
    '& > div:last-child': {
      color: palette.primary.main,
      fontFamily: 'Museo Sans',
      fontWeight: 100,
      fontSize: '20px',

      [breakpoints.down('sm')]: {
        fontSize: '16px'
      }
    }
  },

  coins: {
    '& > img': {
      marginRight: '20px',
      opacity: '0.5',
      [breakpoints.down('sm')]: {
        width: '20px',
        marginRight: '12px'
      }
    }
  },

  connectWallet: {
    background: palette.info.light,
    padding: '5px 40px',
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
        <Box pt={!mobile ? '100px' : '350px'} />

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

        <Box ml={mobile ? '12px' : 0} className={cx(classes.coins)}>
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
          <img src={COIN_CARDANO} alt='cardano coin' />
        </Box>

        <Box mt={!mobile ? '40px' : '20px'} />

        <Box className={cx(classes.connectWallet)}>CONNECT A WALLET</Box>

        {mobile && (
          <Box mt='50px' textAlign='left'>
            <ThemeSwitch />
          </Box>
        )}

        <Box mt={!mobile ? '200px' : '50px'} />

        <Grid container spacing={4}>
          <Grid item xs={12} sm={6} md={4}>
            <TokenCard
              name='LINK-A'
              label='NEW'
              stabilityFee={3}
              ratio={165}
              image={COIN_DANA}
              background={
                dark
                  ? 'linear-gradient(180deg, #3142A3 0%, rgba(49, 66, 163, 0) 118.48%)'
                  : 'linear-gradient(179.9deg, #0D1E7E 0.1%, #4D5EC1 99.98%)'
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
                dark
                  ? 'linear-gradient(180.2deg, #627EFF 0.17%, rgba(77, 97, 210, 0) 116.51%)'
                  : 'linear-gradient(180deg, #2E49C5 0%, #6480FF 106.6%)'
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
                dark
                  ? 'linear-gradient(180deg, #71CEF3 0%, rgba(113, 206, 243, 0) 110%)'
                  : 'linear-gradient(180deg, #0E70CA 0%, #64B2FA 110%)'
              }
            />
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default AdSection
