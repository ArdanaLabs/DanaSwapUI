import React from "react";
import { Box, useMediaQuery, Grid, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { ProfileBox } from "components/Box";

import { Members } from "data";
import { ProfileType } from "components/Box/ProfileBox";

import BG_BLUE_RADIAL from "assets/backgrounds/cyan-gradient.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: ` url(${BG_BLUE_RADIAL}) top -500px left -1000px no-repeat,
                  url(${BG_BLUE_RADIAL}) bottom -300px right -1000px no-repeat`,
    padding: "100px 0",

    [breakpoints.down("xs")]: {
      padding: "20px 0",
    },
  },
  alignStretch: {
    display: "flex",
    alignItems: "stretch",
    flexFlow: "column",
  },
}));

const ProfileSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <Grid container spacing={!mobile ? 5 : 2}>
          {Members.map((profile: ProfileType, index) => (
            <Grid
              item
              key={index}
              xs={6}
              sm={4}
              md={3}
              className={cx(classes.alignStretch)}
            >
              <ProfileBox profile={profile} />
            </Grid>
          ))}
        </Grid>
      </Container>
    </Box>
  );
};

export default ProfileSection;
