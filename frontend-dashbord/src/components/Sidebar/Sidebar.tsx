import React from 'react';
import { useLocation } from 'react-router-dom';
import { Link } from 'react-router-dom';
import { Box, Grid } from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';
import cx from 'classnames';

// import { useDarkModeManager } from 'state/user/hooks';

import { SwitchWithGlider, ThemeSwitch } from 'components';
import SidebarItem from './SidebarItem';
import MainLogo from 'assets/svg/MainLogo.svg';
import HomeIcon from 'assets/svg/Home.svg';
import UserIcon from 'assets/svg/User.svg';
import HandIcon from 'assets/svg/Hand.svg';

// adding account url for the discord and telegram
/*
const insights = [
  {
    title: 'telegram',
    link: '',
    Icon: <CareerIcon />, //telegram invite url
    href: true
  },
  {
    title: 'Careers',
    link: '', // 'discord invite url',
    Icon: <CareerIcon />,
    href: true
  }
];*/

const useStyles = makeStyles(({ palette }) => ({
  rightBorder: {
    // borderRight: `1px solid ${palette.divider}`
  },

  subtitle: {
    marginBottom: 8,
    marginLeft: '1rem',
    fontSize: 10,
    color: palette.text.secondary
  },

  light: {
    background: '-webkit-linear-gradient(270deg, #22329C 0%, #73D6F1 100%)'
  },

  switchContainer: {
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'flex-start',
    justifyContent: 'space-between',
    width: '215',
    height: '162px'
  },

  switchContainerMobile: {
    display: 'flex',
    marginBottom: '4px',
    flexDirection: 'column',
    // backgroundColor: palette.background.paper,
    alignItems: 'flex-start',
    justifyContent: 'space-between',
    width: '100%',
    height: '162px'
  }
}));

export interface SidebarProps {
  mobile?: boolean;
  onHide?: () => void;
}

interface PageIndexing {
  [key: string]: number;
}

const Sidebar: React.FC<SidebarProps> = ({ mobile, onHide }) => {
  // const [darkMode] = useDarkModeManager();
  const classes = useStyles();
  const location = useLocation<{ previous: string }>();
  const { pathname } = location;
  const pageIndexes: PageIndexing = {
    '/home': 0,
    '/pools': 1,
    '/dao': 2
  };
  const state = location.state ? location.state.previous : false;
  const startIndex = state ? pageIndexes[state] : pageIndexes[pathname] || 0;
  const [pageNavigationIndex, setPageNavigationIndex] =
    React.useState(startIndex);

  React.useEffect(() => {
    const currentPage = pageIndexes[pathname];
    setPageNavigationIndex(currentPage);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [pathname]);

  const navigation = [
    {
      title: 'Swap',
      link: '/home',
      Icon: { url: HomeIcon, alt: 'Home' }
    },
    {
      title: 'Pools',
      link: '/pools',
      Icon: { url: HandIcon, alt: 'Hand' }
    },
    {
      title: 'DAO',
      link: '/dao',
      Icon: { url: UserIcon, alt: 'User' }
    }
  ];

  const navigationItems = navigation.map(({ title, link, Icon }, i) => (
    <SidebarItem
      key={i}
      title={title}
      link={link}
      Icon={Icon}
      onHide={onHide}
      // onClick={onClick}
      // disabled={disabled}
    />
  ));

  return (
    <Box
      clone
      width={mobile ? '100%' : '230px'}
      pl={{ sm: 0, md: '15px' }}
      pt={{ sm: 3, md: '30px' }}
      pb={{ sm: 1, md: '15px' }}
      position="relative"
      height={mobile ? 'auto' : 'calc(100vh - 80px)'}
      className={cx({
        [classes.rightBorder]: !mobile,
        [classes.light]: !mobile
      })}
    >
      <Box
        display="flex"
        flexDirection="column"
        justifyContent="space-between"
        style={{ overflowY: 'auto' }}
      >
        <Box>
          {!mobile && (
            <Grid container component={Link} to="/">
              <Box pb={3}>
                <img src={MainLogo} alt="Logo" style={{ marginLeft: '15px' }} />
              </Box>
            </Grid>
          )}
          <Box
            className={
              !mobile ? classes.switchContainer : classes.switchContainerMobile
            }
          >
            {!mobile ? (
              <SwitchWithGlider
                elements={[...navigationItems]}
                defaultIndex={
                  pageNavigationIndex === undefined ? -1 : pageNavigationIndex
                }
                marginBetweenSwitches={0}
                gliderWidth={214}
                gliderHeight={54}
                verticalGlider
              />
            ) : (
              <SwitchWithGlider
                elements={navigationItems}
                defaultIndex={
                  pageNavigationIndex === undefined ? -1 : pageNavigationIndex
                }
                marginBetweenSwitches={0}
                gliderWidth={'100%'}
                gliderHeight={54}
                verticalGlider
              />
            )}
          </Box>
        </Box>

        <Box>
          {/* <Box mb={mobile ? 0 : 2}>
            {insights.map(({ href, title, link, Icon }, i) => (
              <SidebarItem
                key={i}
                href={href}
                title={title}
                link={link}
                Icon={Icon}
              />
            ))}
          </Box> */}
          {!mobile && <ThemeSwitch />}
        </Box>
      </Box>
    </Box>
  );
};

export default Sidebar;
