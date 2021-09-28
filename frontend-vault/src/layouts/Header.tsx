import React from 'react'
import {
  Box,
  useMediaQuery,
  Container
} from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'

import { useIsDarkMode } from 'state/user/hooks'
import { useHistory } from 'react-router-dom'
import { ThemeSwitch, ConnectWallet } from 'components'
import DANA_LOGO_BLACK from 'assets/image/DANA-LOGO-BLACK.png'
import DANA_LOGO_WHITE from 'assets/image/DANA-LOGO-WHITE.png'

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: 'fixed',
    top: 0,
    background: palette.background.default,
    zIndex: 100,
    width: '100%',
    filter: 'drop-shadow(0px 15px 15px rgba(0, 0, 0, 0.05))',
    mixBlendMode: 'normal'
  },

  container: {
    display: 'flex',
    justifyContent: 'space-between',
    alignItems: 'center',
    transition: 'background .2s ease-in'
  },

  logo: {
    paddingLeft: '10px',
    display: 'flex',
    alignItems: 'center',
    cursor: 'pointer',
    '& img': {
      padding: '20px 10px',
      width: '60px',

      [breakpoints.down('sm')]: {
        width: '50px',
      }
    }
  },
  
  toolbar: { 
    display: 'flex',
    alignItems: 'center'
  }
}))

const Header: React.FC = () => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down('sm'))
  const dark = useIsDarkMode()
  const classes = useStyles({ dark, mobile })
  const history = useHistory()

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box className={cx(classes.container)}>
          <Box className={cx(classes.logo)} onClick={() => history.push('/')}>
            <img
              src={
                theme.palette.type === 'dark'
                  ? DANA_LOGO_WHITE
                  : DANA_LOGO_BLACK
              }
              alt='DANA Logo'
            />
          </Box>

          <Box className={cx(classes.toolbar)}>
            {!mobile && <ThemeSwitch />}
            <ConnectWallet />
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default Header
