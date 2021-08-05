import React from 'react';
import { useHistory } from 'react-router-dom';
import { Box, Grid, Typography, Divider } from '@material-ui/core';
import { makeStyles, useTheme } from '@material-ui/core/styles';
import cx from 'classnames';
import { useDarkModeManager } from 'state/user/hooks';

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  divider: {
    height: '48px'
  },

  account: {
    padding: 0,
    height: 45,
    border: `1px solid ${palette.divider}`,
    backgroundColor: palette.background.default,
    boxShadow: (props: any) =>
      props.darkMode ? '0px 2px 5px rgba(0, 0, 0, 0.0746353)' : 'none',
    borderRadius: 12,
    cursor: 'pointer',

    '& a': {
      textDecoration: 'none'
    },

    '&> div:hover:not(:active)': {
      '&> svg path': {
        fill: palette.text.primary
      },

      '& p': {
        color: palette.text.primary
      }
    }
  },

  accountInfo: {
    display: 'flex',
    alignItems: 'center',
    paddingLeft: 10,
    paddingRight: 6
  },

  accountMobile: {
    height: 45,
    width: '100%',
    border: `1px solid ${palette.divider}`,
    borderRadius: 12,

    '& $disconnect': {
      width: 43,
      flex: 'none'
    }
  },

  fullWidth: {
    margin: 0,
    width: '100%'
  },

  home: {
    display: 'flex',
    alignItems: 'center',
    padding: '0 8px',
    '& p': {
      fontSize: 14,
      lineHeight: '18px',
      color: palette.text.secondary
    }
  },

  container: {
    overflow: 'hidden',
    '& > div:hover, & > div:active': {
      background: 'linear-gradient(121.21deg, #5294FF 7.78%, #1EFF78 118.78%)',
      '& p': {
        color: (props: any) =>
          props.darkMode ? 'black !important' : 'white !important',
        textFillColor: 'unset',
        background: 'transparent'
      },
      '& svg path': {
        fill: (props: any) =>
          props.darkMode ? 'black !important' : 'white !important'
      }
    },
    '& $accountInfo > div': {
      display: 'flex',
      flexDirection: 'column',
      justifyContent: 'center',
      marginLeft: 5,
      '& p': {
        fontSize: 14,
        fontWeight: 'bold',
        lineHeight: '18px',
        margin: 0
      }
    }
  }
}));

interface AccountButtonsProps {
  mobile?: boolean;
  onHide?: () => void;
}

const AccountButtons: React.FC<AccountButtonsProps> = ({ mobile, onHide }) => {
  const history = useHistory();
  const theme = useTheme();
  const [darkMode] = useDarkModeManager();
  const classes = useStyles({ darkMode, mobile });

  return (
    <Grid container alignItems="center" justify="flex-end">
      <Box
        display="flex"
        className={cx(classes.account, classes.container)}
        width={mobile ? 1 : 'auto'}
        my={mobile ? 1.25 : 0}
        mx={mobile ? 1.25 : 1.25}
      >
        <Box
          height={1}
          className={classes.accountInfo}
          flex={1}
          display="flex"
          justifyContent="center"
          onClick={() => {
            history.push('/trading-competition');

            if (onHide) {
              onHide();
            }
          }}
        ></Box>
        <Box
          height={1}
          borderLeft={1}
          borderColor={theme.palette.divider}
          className={classes.home}
          onClick={() => {
            history.push('/home');

            if (onHide) {
              onHide();
            }
          }}
        >
          <Typography>Home</Typography>
        </Box>
      </Box>

      {mobile && <Divider className={classes.fullWidth} />}
    </Grid>
  );
};

export default AccountButtons;
