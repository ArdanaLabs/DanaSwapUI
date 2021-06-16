import React from 'react';
import { Box, Grid, useMediaQuery } from '@material-ui/core';
import { makeStyles, useTheme } from '@material-ui/core/styles';

import { useIsDarkMode } from 'state/user/hooks';

const useStyles = makeStyles(({ palette }) => ({
  footer: {
    height: '100%'
  }
}));

const Footer: React.FC = () => {
  const { palette, breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down('xs'));
  const classes = useStyles({ dark, mobile });

  return (
    <Box
      height={mobile ? 99 : 42}
      width={1}
      borderTop={1}
      borderColor={palette.divider}
      style={mobile ? { backgroundColor: palette.background.paper } : {}}
    >
      <Grid
        container
        justify="space-between"
        alignItems="center"
        className={classes.footer}
      >
        This is the footer for the ardana
      </Grid>
    </Box>
  );
};

export default Footer;
