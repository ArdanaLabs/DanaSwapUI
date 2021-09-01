import React from 'react'
import { Box, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'
import { useIsDarkMode } from 'state/user/hooks'

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    borderRadius: '30px',
    padding: '20px',
    color: '#FFFFFF',
    display: 'flex'
  },
  typographyPrimary: {
    fontFamily: 'Brandon Grotesque',
    fontStyle: 'normal',
    fontWeight: 900
  },
  typographySecondary: {
    fontFamily: 'Museo Sans',
    fontStyle: 'normal',
    fontWeight: 100
  },
  image: {
    marginTop: '-80px'
  },
  title: {
    fontSize: '50px',
    lineHeight: '110%',
    marginBottom: '20px'
  },
  content: {
    fontSize: '18px',
    lineHeight: '115%'
  }
}))

export interface SwapButtonProps {
  image: string
  title: string
  content: string
  background: string
}

const SwapButton: React.FC<SwapButtonProps> = ({
  image,
  title,
  content,
  background
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const responsive = useMediaQuery(breakpoints.down('md'))
  const classes = useStyles({ dark, mobile })

  return (
    <Box
      className={cx(classes.root)}
      style={{ background: background }}
      flexDirection={responsive ? 'column' : 'row'}
      textAlign={responsive ? 'center' : 'left'}
    >
      <Box className={cx(classes.image)}>
        <img src={image} alt='' />
      </Box>
      <Box display='flex' flexDirection='column' my='30px'>
        <Box className={cx(classes.typographyPrimary, classes.title)}>
          {title}
        </Box>
        <Box className={cx(classes.typographySecondary, classes.content)}>
          {content}
        </Box>
      </Box>
    </Box>
  )
}

export default SwapButton
