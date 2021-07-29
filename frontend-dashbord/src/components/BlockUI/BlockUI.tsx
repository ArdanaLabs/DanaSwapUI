import { Box } from '@material-ui/core'
import React from 'react'
import { css } from '@emotion/react'
import { useLoading } from 'state/loader/hooks'

import { BeatLoader } from 'react-spinners'

const override = css`
  display: block;
  margin: 0 auto;
  border-color: white;
`

const BlockUI: React.FC = () => {
  const { loading } = useLoading()
  return (
    <Box
      position='fixed'
      width='100vw'
      height='100vh'
      display={loading ? 'flex' : 'none'}
      justifyContent='center'
      alignItems='center'
      zIndex={10000}
      style={{ backgroundColor: 'black', opacity: 0.8 }}
    >
      <BeatLoader
        color={'#FFFFFF'}
        loading={loading}
        css={override}
        size={50}
      />
    </Box>
  )
}

export default BlockUI
