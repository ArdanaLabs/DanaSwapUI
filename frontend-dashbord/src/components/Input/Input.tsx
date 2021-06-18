import React from 'react'
import { Box, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'
import { useIsDarkMode } from 'state/user/hooks'

const useStyles = makeStyles(({ palette }) => ({
  input: {
    '& input': {
      borderRadius: '5px',
      border: 'unset',
      color: '#C4C4C4',
      fontFamily: 'Futura,Trebuchet MS,Arial,sans-serif',
      '&:focus-visible': {
        outline: 'unset'
      }
    }
  }
}))

export interface InputProps {
  value?: any,
  placeholder?: any,
  className?: any
}

const Input: React.FC<InputProps> = ({ value = '', className, placeholder }) => {
  const { palette, breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })
  return (
    <Box className={cx(classes.input)}>
      <input placeholder={placeholder} value={value} className={className}></input>
    </Box>
  )
}

export default Input
