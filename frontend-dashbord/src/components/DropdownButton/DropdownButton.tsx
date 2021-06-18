import React from 'react'
import { Box, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'
import { useIsDarkMode } from 'state/user/hooks'
import Arrow from 'assets/svg/arrow.svg'

const useStyles = makeStyles(({ palette }) => ({
  dropdown: {
    cursor: 'pointer',
    color: palette.common.white
  },
  arrow: {
    width: '12px',
    height: '12px',
    transition: 'all .2s ease-in-out'
  },
  arrowMobile: {
    width: '10px',
    height: '10px'
  },
  isOpen: {
      transform: 'rotate(180deg)',
      marginTop: '-3px'
  }
}))

const defaultSize = {
  desktop: '18px',
  mobile: '16px'
}

export interface DropdownButtonProps {
    isOpen?: boolean
}

const DropdownButton: React.FC<DropdownButtonProps> = ({isOpen = false}) => {
  const { palette, breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })
  return (
    <Box
      width={mobile ? defaultSize.mobile : defaultSize.desktop}
      height={mobile ? defaultSize.mobile : defaultSize.desktop}
      borderRadius={'100%'}
      bgcolor={palette.primary.main}
      p={'4px 3px'}
      pt={isOpen ? '5px' : '4px'}
      display={'flex'}
      className={cx(classes.dropdown)}
    >
      <img
        src={Arrow}
        alt={'>'}
        className={cx(classes.arrow, { [classes.arrowMobile]: mobile, [classes.isOpen]: isOpen })}
      ></img>
    </Box>
  )
}

export default DropdownButton
