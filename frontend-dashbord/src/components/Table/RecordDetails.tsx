import React, { Fragment } from 'react'
import { Box, Grid, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'
import { useIsDarkMode } from 'state/user/hooks'

const useStyles = makeStyles(({ palette }) => ({
  header: {
    fontSize: '18px',
    color: palette.text.primary,
    marginBottom: '10px',
    marginTop: '12px'
  }
}))

export interface RecordProps {
  data?: any
}

const Record: React.FC<RecordProps> = ({ data }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })

  return (
    <Fragment>
      <Grid container item sm={6} xs={6} style={{ alignItems: 'stretch' }}>
        <Grid
          container
          item
          sm={6}
          xs={12}
          style={{
            paddingRight: mobile ? '0px' : '12px',
            paddingLeft: mobile ? '22px' : 'unset',
            display: mobile ? 'block' : 'flex',
            alignItems: 'center'
          }}
        >
          {mobile && <Box className={cx(classes.header)}>Base APY</Box>}
          <Box>{data.baseAPY}</Box>
        </Grid>
        <Grid
          container
          item
          sm={6}
          xs={12}
          style={{
            paddingLeft: mobile ? '22px' : '12px',
            wordBreak: 'break-all',
            display: mobile ? 'block' : 'flex',
            alignItems: 'center',
            paddingBottom: mobile ? '15px' : 'unset'
          }}
        >
          {mobile && <Box className={cx(classes.header)}>RewardsAPY</Box>}
          <Box>{data.rewardsAPY}</Box>
        </Grid>
      </Grid>
      <Grid
        container
        item
        sm={2}
        xs={6}
        style={{ display: mobile ? 'block' : 'flex', alignItems: 'center' }}
      >
        {mobile && <Box className={cx(classes.header)}>Volumn</Box>}
        <Box>{data.volumn}</Box>
      </Grid>
    </Fragment>
  )
}

export default Record
