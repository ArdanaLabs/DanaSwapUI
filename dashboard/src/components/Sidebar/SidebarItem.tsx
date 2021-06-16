import React from 'react';
import { useHistory, useLocation } from 'react-router-dom';
import { Box, Typography, Tooltip } from '@material-ui/core';
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
      fill: palette.primary.main
    },
    '& .MuiTypography-root': {
      fontWeight: 400,
      lineHeight: '14px',
      fontSize: '14px',
      color: palette.primary.main
    },
    '&:hover': {
      '& svg path': {
        fill: '#006DFF'
      },
      '& .MuiTypography-root': {
        fontWeight: 400,
        fontSize: '14px',
        color: '#006DFF'
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

  const WrapperComponent = disabled ? Tooltip : React.Fragment;

  return (
    <WrapperComponent title="Disabled on testnet">
      <Box
        display="flex"
        alignItems="center"
        justifyContent="flex-start"
        paddingLeft="30px"
        width={!mobile ? '214px' : '100%'}
        height="54px"
        className={cx(
          classes.inactiveSwitch,
          activeCondition && classes.activeSwitch,
          disabled && classes.disabled
        )}
        onClick={disabled ? () => {} : handleClick}
      >
        {Icon}
        <Typography>{title}</Typography>
      </Box>
    </WrapperComponent>
  );
};

export default SidebarItem;
