import React from 'react'
import { Box, useMediaQuery } from '@material-ui/core'
import { useTheme } from '@material-ui/core/styles'
import LandingImage from 'components/LandingImage'
import { pools } from 'data'
import PoolsPanel from 'components/Table'

const Pools: React.FC = () => {
  const { palette, breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down('xs'))

  return (
    <Box style={mobile ? { backgroundColor: palette.background.paper } : {}}>
      <LandingImage url={'HOME > POOLS'} title={'Pools'} />
      <PoolsPanel data={pools}></PoolsPanel>
    </Box>
  )
}

export default Pools
