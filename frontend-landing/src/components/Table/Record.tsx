import React from 'react'
import { Box, Grid, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'
import { useIsDarkMode } from 'state/user/hooks'
import { DropdownButton } from 'components/Button'
import Collapse from '@material-ui/core/Collapse'
import RecordDetails from './RecordDetails'

const useStyles = makeStyles(({ palette }) => ({
  record: {
    display: 'flex',
    backgroundColor: "transparent",
    color: palette.secondary.main,
    borderRadius: '5px',
    padding: '17px 26px',
    borderTop: "1px solid #E5E5E5",
  },
  recordMobile: {
    margin: '8px 0',
    padding: '20px 15px'
  }
}))

export interface RecordProps {
  data: any;
  widthRatio?: any;
}

const Record: React.FC<RecordProps> = ({ data, widthRatio }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })
  const [isOptionOpen, setIsOptionOpen] = React.useState(false)

  const onToggleOptions = () => {
    setIsOptionOpen(prev => !prev)
  }

  return (
    <Box className={cx(classes.record, mobile && classes.recordMobile)}>
      <Grid container spacing={1} style={{ alignItems: 'stretch' }}>
        <Grid
          container
          item
          sm={widthRatio}
          xs={12}
          onClick={mobile ? onToggleOptions : () => {}}
          style={{ paddingRight: mobile ? '35px' : 0, position: 'sticky' }}
        >
          <img
            src={data.pool.icon}
            alt={'coin'}
            style={{ marginRight: '15px' }}
          ></img>
          <Box
            display={'flex'}
            flexDirection={'column'}
            justifyContent={'center'}
          >
            <Box>{data.pool.currency}</Box>
            <Box>{data.pool.description}</Box>
          </Box>
          {mobile && (
            <DropdownButton
              style={{
                transform: 'scale(1.5)',
                position: 'absolute',
                right: '18px',
                top: '27px'
              }}
              isOpen={isOptionOpen}
            />
          )}
        </Grid>
        {mobile ? (
          <Collapse in={isOptionOpen}>
            <Grid container spacing={3} style={{ alignItems: 'stretch' }}>
              <RecordDetails data={data}></RecordDetails>
            </Grid>
          </Collapse>
        ) : (
          <RecordDetails data={data}></RecordDetails>
        )}
      </Grid>
    </Box>
  )
}

export default Record
