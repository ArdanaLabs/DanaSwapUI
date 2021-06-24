import React from "react";
import { Box, useMediaQuery, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";

import LOGO_APPLE from "assets/img/landing/logos/logo_apple.svg";
import LOGO_MICROSOFT from "assets/img/landing/logos/logo_microsoft.svg";
import LOGO_BARCLAYS from "assets/img/landing/logos/logo_barclays.svg";
import LOGO_MINA from "assets/img/landing/logos/logo_mina.svg";
import LOGO_RAKUTEN from "assets/img/landing/logos/logo_rakuten.svg";
import LOGO_IG from "assets/img/landing/logos/logo_ig.svg";
import LOGO_CARDANO from "assets/img/landing/logos/logo_cardano.svg";
import LOGO_EMURGO from "assets/img/landing/logos/logo_emurgo.svg";

const TopNotchTeams = [
  {
    logo: LOGO_APPLE,
  },
  {
    logo: LOGO_MICROSOFT,
  },
  {
    logo: LOGO_BARCLAYS,
  },
  {
    logo: LOGO_MINA,
  },
  {
    logo: LOGO_RAKUTEN,
  },
  {
    logo: LOGO_IG,
  },
  {
    logo: LOGO_CARDANO,
  },
  {
    logo: LOGO_EMURGO,
  },
];

const useStyles = makeStyles(({ palette }) => ({
  bg: {
    background: "#FFFFFF",
    padding: "50px 0",
  },
}));

const TopNotchSection: React.FC = () => {
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
          Ardana with our top-notch team
        </Box>

        <Box
          textAlign="center"
          fontSize={!mobile ? "18px" : "16px"}
          color="#202020"
          fontWeight="300"
          lineHeight="27px"
          p="20px"
        >
          Our team is led by accomplished entrepreneurs and top-notch engineers
          from well-known companies such as Apple, Microsoft, Barclays, Cardano,
          Emurgo, and IG Index.
        </Box>

        <Box
          display="flex"
          flexWrap="wrap"
          alignItems="center"
          justifyContent="center"
          p="20px"
        >
          {TopNotchTeams.map((team, index) => (
            <Box
              key={index}
              p={!mobile ? "0px 30px" : "10px"}
              width={!mobile ? "auto" : "50%"}
              textAlign="center"
            >
              <img
                src={team.logo}
                alt=""
                width="100%"
                height="100%"
                style={{ maxWidth: "max-content" }}
              />
            </Box>
          ))}
        </Box>
      </Container>
    </Box>
  );
};

export default TopNotchSection;
