import React from "react";
import { Box, useMediaQuery, Grid, Container, Link } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import ScrollAnimation from "react-animate-on-scroll";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { ProfileBox } from "components/Box";

import { ProfileList } from "data";
import { ProfileType } from "components/Box/ProfileBox";

const useStyles = makeStyles(({ palette }) => ({
  bg: {
    background: palette.background.default,
    padding: "100px 20px 100px 20px",
  },
  mobile_bg: {
    background: palette.background.default,
    padding: "20px",
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
    <Box
      className={cx({ [classes.bg]: !mobile }, { [classes.mobile_bg]: mobile })}
    >
      <Container>
        <Grid container spacing={5}>
          {ProfileList.map((profile, index) => (
            <Grid
              item
              key={index}
              xs={6}
              sm={4}
              md={3}
              className={cx(classes.alignStretch)}
            >
              <ProfileBox profile={profile} />
              {/* <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box display="flex" alignItems="stretch">
                  {profile.linkedin && (
                    <Link href={profile.linkedin} target="_blank">
                      <img
                        src={ICO_Linkedin}
                        alt="linkedin"
                        style={{ maxWidth: "max-content" }}
                        width={!mobile ? "100%" : "75%"}
                      />
                    </Link>
                  )}
                  {profile.github && (
                    <Link href={profile.github} target="_blank">
                      <img
                        src={ICO_Github}
                        alt="github"
                        style={{ maxWidth: "max-content" }}
                        width={!mobile ? "100%" : "75%"}
                      />
                    </Link>
                  )}
                </Box>
              </ScrollAnimation> */}
            </Grid>
          ))}
        </Grid>
      </Container>
    </Box>
  );
};

export default ProfileSection;
