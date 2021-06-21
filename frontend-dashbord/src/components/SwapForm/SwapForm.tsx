import React from 'react'
import { Box, Grid, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'
import cx from 'classnames'
import { useIsDarkMode } from 'state/user/hooks'
import Input from 'components/Input'

const useStyles = makeStyles(({ palette }) => ({
  chooseCoin: {
    height: '28px',
    width: '148px',
    color: palette.text.secondary,
    fontSize: '12px',
    textAlign: 'center',
    backgroundColor: palette.common.white,
    borderRadius: '100px',
    fontWeight: 'bold',
    lineHeight: '28px'
  },
  chooseCoinMobile: {
    height: '25.5px',
    lineHeight: '25.5px',
    width: '136px'
  },
  coinBox: {
    width: '150px',
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'center',
    order: 1
  },
  coinBoxMobile: {
    order: 0
  },
  panel: {
    display: 'flex',
    float: 'right',
    alignItems: 'center'
  },
  panelMobile: {
    display: 'flex',
    flexDirection: 'column'
  },
  swapInput: {
    height: '41px',
    marginTop: '20px',
    padding: '7px 17px',
    fontSize: '18px',
    width: '100%',
    maxWidth: '250px'
  },
  swapInputMobile: {}
}))

export interface SwapFormProps {}

const SwapForm: React.FC<SwapFormProps> = () => {
  const { palette, breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })

  return (
    <Grid container spacing={mobile ? 1 : 2}>
      <Grid container item xs={6}>
        <Box
          position={'sticky'}
          width={'100%'}
          bgcolor={palette.secondary.main}
          pt={mobile ? '15px' : '37px'}
          pb={mobile ? '28px' : '37px'}
          pl={mobile ? '14px' : '37px'}
          pr={mobile ? '14px' : '37px'}
          borderRadius={'5px'}
          className={cx(classes.panel, {
            [classes.panelMobile]: mobile
          })}
        >
          <Box
            className={cx(classes.coinBox, {
              [classes.coinBoxMobile]: mobile
            })}
          >
            <Box
              width={mobile ? '81px' : '138px'}
              height={mobile ? '81px' : '138px'}
              bgcolor={palette.common.white}
              borderRadius={'100%'}
            ></Box>
            <Box
              className={cx(classes.chooseCoin, {
                [classes.chooseCoinMobile]: mobile
              })}
              mt={mobile ? '15px' : '24px'}
            >
              Choose Coin
            </Box>
          </Box>
          <Box
            width={'100%'}
            pr={mobile ? '0px' : '20px'}
            mt={mobile ? '20px' : 0}
            textAlign={mobile ? 'center' : 'auto'}
          >
            <Box
              fontSize={mobile ? '13px' : '18px'}
              color={palette.text.primary}
            >
              Swap Form
            </Box>
            <Box
              fontSize={mobile ? '11px' : '14px'}
              color={palette.text.secondary}
            >
              Please enter your desired amount
            </Box>
            <Input
              value={'0.00'}
              className={cx(classes.swapInput, {
                [classes.swapInputMobile]: mobile
              })}
            ></Input>
          </Box>
        </Box>
      </Grid>
      <Grid container item xs={6}>
        <Box
          position={'sticky'}
          width={'100%'}
          bgcolor={palette.secondary.main}
          pt={mobile ? '15px' : '37px'}
          pb={mobile ? '28px' : '37px'}
          pl={mobile ? '14px' : '37px'}
          pr={mobile ? '14px' : '37px'}
          borderRadius={'5px'}
          className={cx(classes.panel, {
            [classes.panelMobile]: mobile
          })}
        >
          <Box
            className={cx(classes.coinBox, {
              [classes.coinBoxMobile]: mobile
            })}
          >
            <Box
              width={mobile ? '81px' : '138px'}
              height={mobile ? '81px' : '138px'}
              bgcolor={palette.common.white}
              borderRadius={'100%'}
            ></Box>
            <Box
              className={cx(classes.chooseCoin, {
                [classes.chooseCoinMobile]: mobile
              })}
              mt={mobile ? '15px' : '24px'}
            >
              Choose Coin
            </Box>
          </Box>
          <Box
            width={'100%'}
            pr={mobile ? '0px' : '20px'}
            mt={mobile ? '20px' : 0}
            textAlign={mobile ? 'center' : 'auto'}
          >
            <Box
              fontSize={mobile ? '13px' : '18px'}
              color={palette.text.primary}
            >
              Swap To
            </Box>
            <Box
              fontSize={mobile ? '11px' : '14px'}
              color={palette.text.secondary}
            >
              Please view the swapped amount here:
            </Box>
            <Input
              value={'0.00'}
              className={cx(classes.swapInput, {
                [classes.swapInputMobile]: mobile
              })}
            ></Input>
          </Box>
        </Box>
      </Grid>
    </Grid>
  )
}

export default SwapForm
