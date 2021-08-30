import React, { useState } from 'react'
import {
  Box,
  Link,
  IconButton,
  Drawer,
  useMediaQuery,
  Container
} from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import Hamburger from 'hamburger-react'
import cx from 'classnames'

import { useIsDarkMode } from 'state/user/hooks'
import { useHistory, useLocation } from 'react-router-dom'
import ThemeSwitch from 'components/ThemeSwitch'
import { Button } from 'components/Button'

import { navList } from 'data'
import DANA_LOGO_BLACK from 'assets/image/DANA-LOGO-BLACK.png'
import DANA_LOGO_WHITE from 'assets/image/DANA-LOGO-WHITE.png'

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: 'fixed',
    top: 0,
    background: palette.background.default,
    zIndex: 100,
    width: '100%',
    paddingBottom: '10px',
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
  const { pathname } = useLocation<{ previous: string }>()

  const [openMenu, setOpenMenu] = useState(false)

  const toggleMenu = () => {
    setOpenMenu(prev => !prev)
  }

  const isActiveURL = (link: string): boolean => {
    return pathname.indexOf(link) > -1
  }

  const onConnectWallet = (event: any) => {
    console.log('connect wallet button clicked!')
  }

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
            <ThemeSwitch />
            <Box>
              
            </Box>
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default Header
