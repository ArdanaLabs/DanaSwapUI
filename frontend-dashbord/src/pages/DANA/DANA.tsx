import React from 'react'
import { Box, useMediaQuery } from '@material-ui/core'
import { useTheme } from '@material-ui/core/styles'
import LandingImage from 'components/LandingImage'

const DANA: React.FC = () => {
  const { palette, breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down('xs'))

  return (
    <Box style={mobile ? { backgroundColor: palette.background.paper } : {}}>
      <LandingImage url={'HOME > DAO'} title={'DAO'} />
    </Box>
  )
}

export default DANA
