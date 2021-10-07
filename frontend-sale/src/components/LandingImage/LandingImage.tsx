import React from 'react'
import { Box, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'
import { useIsDarkMode } from 'state/user/hooks'
import LandingImage1 from 'assets/landing_image1.png'
import LandingImage2 from 'assets/landing_image2.png'

const useStyles = makeStyles(({ palette }) => ({
  landingImage: {
    maxHeight: '380px',
    minHeight: '253px',
    width: '100%',
    objectFit: 'cover'
  },
  coveredImaged: {
    position: 'absolute',
    top: '0px',
    left: '0px'
  },
  url: {
    fontSize: '14px',
    fontFamily: 'Poppins',
    margin: 'unset',
    color: palette.text.hint,
    transition: "color .3s ease-in",
  },
  title: {
    fontSize: '36px',
    margin: 'unset',
    marginTop: '10px',
    color: palette.text.hint,
    transition: "color .3s ease-in",
  }
}))

export interface LandingImageProps {
  url?: string
  title?: string
}

const LandingImage: React.FC<LandingImageProps> = ({ url, title }) => {
  const { palette, breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })

  return (
    <Box position={'sticky'} width={'100%'}>
      <img src={LandingImage1} alt='' className={cx(classes.landingImage)} />
      <img
        src={LandingImage2}
        alt=''
        className={cx(classes.landingImage, classes.coveredImaged)}
      />
      <Box
        mt={mobile ? '32px' : '80px'}
        ml={mobile ? '28px' : '48px'}
        color={palette.common.white}
        position={'absolute'}
        style={{ top: '0px', left: '0px' }}
      >
        <p className={cx(classes.url)}>{url}</p>
        <p className={cx(classes.title)}>{title}</p>
      </Box>
    </Box>
  )
}

export default LandingImage
