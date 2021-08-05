import React from 'react';
import { useHistory, useLocation } from 'react-router-dom';
import { Box, Typography } from '@material-ui/core';
import { useTheme } from '@material-ui/core/styles';
import useMediaQuery from '@material-ui/core/useMediaQuery';

import { makeStyles } from '@material-ui/core/styles';
import cx from 'classnames';

const useStyles = makeStyles(({ palette }) => ({
  inactiveSwitch: {
    cursor: 'pointer',
    '& svg': {
      marginRight: 10
    },
    '& svg path': {
      fill: palette.common.white
    },
    '& .MuiTypography-root': {
      fontWeight: 400,
      lineHeight: '14px',
      fontSize: '14px',
      color: palette.text.hint,
      marginLeft: '15px',
    },
    '&:hover': {
      '& $menuIcon': {
        background: palette.primary.main
      },
      '& .MuiTypography-root': {
        fontWeight: 500,
        fontSize: '14px',
        color: '#006DFF',
      }
    }
  },
  activeSwitch: {
    cursor: 'default',
    '& svg path': {
      fill: '#006DFF'
    },
    '& .MuiTypography-root': {
      color: '#006DFF',
      fontWeight: 700
    },
    '&:hover': {
      '& svg path': {
        fill: '#006DFF'
      },
      '& .MuiTypography-root': {
        color: '#006DFF',
        fontWeight: 700
      }
    },
    '& $menuIcon': {
      background: palette.primary.main
    }
  },
  disabled: {
    cursor: 'default',

    '& svg path': {
      fill: palette.text.secondary
    },

    '& .MuiTypography-root': {
      color: palette.text.secondary
    },

    '&:hover': {
      '& svg path': {
        fill: palette.text.secondary
      },
      '& .MuiTypography-root': {
        color: palette.text.secondary
      }
    }
  },
  menuIcon: {
    width: '20px',
    height: '20px',
    backgroundColor: palette.text.hint,
    WebkitMaskRepeat: 'no-repeat',
    WebkitMaskSize: '100%',
    transition: 'background .3s ease-in',
  },
  menuItem: {
    transition: 'color .3s ease-in',
  }
}));

export interface SidebarItemProps {
  title: string;
  link?: string | undefined;
  Icon: any;
  href?: boolean;
  disabled?: boolean;
  onHide?: () => void;
  activeCondition?: any;
  onClick?: (() => void) | undefined;
}

const SidebarItem: React.FC<SidebarItemProps> = ({
  title,
  link,
  Icon,
  href,
  disabled,
  onClick,
  onHide,
  activeCondition
}) => {
  const location = useLocation();
  const active = location.pathname === link;
  const classes = useStyles({ active });
  const theme = useTheme();
  const mobile = useMediaQuery(theme.breakpoints.down('sm'));
  const history = useHistory();

  const handleClick = (event: React.MouseEvent<HTMLElement>) => {
    if (!href) {
      if (link) {
        history.push(link, { previous: location.pathname });
        if (onHide) {
          onHide();
        }
      } else if (onClick) {
        onClick();
      }
    } else {
      window.open(link, '_blank');
    }
  };

  // const WrapperComponent = disabled ? Tooltip : React.Fragment;
  // const WrapperComponent = Tooltip;

  return (
    // <WrapperComponent title="Disabled on testnet">
      <Box
        display="flex"
        alignItems="center"
        justifyContent="flex-start"
        paddingLeft="30px"
        width={!mobile ? '215px' : '100vw'}
        height="54px"
        className={cx(
          classes.inactiveSwitch,
          active && classes.activeSwitch,
          disabled && classes.disabled
        )}
        onClick={disabled ? () => {} : handleClick}
      >
        <div className={cx(
          classes.menuIcon
        )} style={{maskImage: `url(${Icon.url})`, WebkitMaskImage: `url(${Icon.url})`}}></div>
        <Typography className={cx(classes.menuItem)}>{title}</Typography>
      </Box>
    // </WrapperComponent>
  );
};

export default SidebarItem;
