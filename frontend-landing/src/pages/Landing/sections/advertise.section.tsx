import React from 'react'
import { Box, Container, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'

import { useIsDarkMode } from 'state/user/hooks'

import BACKGROUND_COIN from 'assets/image/BACKGROUND-COIN.png'
import BACKGROUND_WAVE from 'assets/image/BACKGROUND-WAVE.png'
import COIN_CARDANO from 'assets/image/COIN1.png'

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    paddingBottom: '500px',
    background: `url(${BACKGROUND_COIN}) right top no-repeat, url(${BACKGROUND_WAVE}) left top no-repeat`,
    backgroundSize: '500px, cover'
  },

  description: {
    '& > div:first-child': {
      color: palette.primary.main,
      fontFamily: 'Brandon Grotesque',
      fontWeight: 700,
      fontSize: '50px',
      paddingBottom: '20px',
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
      opacity: '0.5',      
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
    fontWeight: 700,
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

        <Box mt="50px" />

        <Box className={cx(classes.coins)}>
          <img src={COIN_CARDANO} alt="cardano coin"/>
          <img src={COIN_CARDANO} alt="cardano coin"/>
          <img src={COIN_CARDANO} alt="cardano coin"/>
          <img src={COIN_CARDANO} alt="cardano coin"/>
          <img src={COIN_CARDANO} alt="cardano coin"/>
          <img src={COIN_CARDANO} alt="cardano coin"/>
          <img src={COIN_CARDANO} alt="cardano coin"/>
        </Box>

        <Box mt="40px" />

        <Box className={cx(classes.connectWallet)}>
          CONNECT A WALLET
        </Box>

        
      </Container>
    </Box>
  )
}

export default AdSection
