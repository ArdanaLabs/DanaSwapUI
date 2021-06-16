import React from 'react';
import { Box, Grid, useMediaQuery } from '@material-ui/core';
import { makeStyles, useTheme } from '@material-ui/core/styles';

import { useIsDarkMode } from 'state/user/hooks';

const useStyles = makeStyles(({ palette }) => ({
  homeContainer: {
    height: '100%'
  }
}));

const Home: React.FC = () => {
  const { palette, breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down('xs'));
  const classes = useStyles({ dark, mobile });

  return (
    <Box
      borderTop={1}
      borderColor={palette.divider}
      style={mobile ? { backgroundColor: palette.background.paper } : {}}
    >
      <Grid container className={classes.homeContainer}>
        This is the Home Container
      </Grid>
    </Box>
  );
};

export default Home;
