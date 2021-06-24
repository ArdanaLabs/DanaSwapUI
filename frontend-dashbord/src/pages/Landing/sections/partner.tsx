import React from "react";
import { Box, useMediaQuery, Grid, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";

import MLABS from "assets/img/landing/logos/MLABS.svg";
import PSYSS from "assets/img/landing/logos/Platonic-Systems.svg";

const useStyles = makeStyles(({ palette }) => ({
  bg: {
    background: "#F5F5F5",
    padding: "20px",
  },
}));

const PartnerSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <Box
          textAlign="center"
          fontSize={!mobile ? "64px" : "48px"}
          color="#202F9A"
          fontWeight="900"
          p="20px"
        >
          Featured Partners
        </Box>

        <Grid
          container
          style={{ padding: "20px" }}
          alignItems="center"
          spacing={3}
        >
          <Grid item xs={12} sm={6}>
            <Box textAlign="center">
              <img
                src={PSYSS}
                alt="Platonic Systems"
                width="100%"
                style={{ maxWidth: "max-content" }}
              />
            </Box>
          </Grid>
          <Grid item xs={12} sm={6}>
            <Box textAlign="center">
              <img
                src={MLABS}
                alt="MLABS"
                width="100%"
                style={{ maxWidth: "max-content" }}
              />
            </Box>
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default PartnerSection;
