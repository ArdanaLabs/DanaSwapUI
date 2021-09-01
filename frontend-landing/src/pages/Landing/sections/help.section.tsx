import React from 'react'
import { Box, Container, Grid, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'

import { useIsDarkMode } from 'state/user/hooks'
import { HelpCard } from 'components'

import IMG_DANACOIN from 'assets/image/CIRCLE-DANACOIN.png'
import IMG_QUESTION from 'assets/image/CIRCLE-QUESTION.png'

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {}
}))

const AdSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container spacing={5}>
          <Grid item xs={12} sm={6}>
            <HelpCard
              image={IMG_DANACOIN}
              title='Dana Coin'
              content='Buy, send and manage your Dana Coin all in one place. Grow your Dana, and access plenty of providers.'
              background='linear-gradient(180deg, #1A235B 0%, rgba(49, 66, 163, 0) 118.48%)'
            />
          </Grid>
          <Grid item xs={12} sm={6}>
            <HelpCard
              image={IMG_QUESTION}
              title='Got questions?'
              content='Learn more about Dana Coin, Danaswap and Stablecoin Vaults by visiting our FAQs page.'
              background='linear-gradient(180deg, #1A235B 0%, rgba(49, 66, 163, 0) 118.48%)'
            />
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default AdSection
