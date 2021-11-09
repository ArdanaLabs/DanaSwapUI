import React, { useEffect, useState } from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { BrandAssetBox } from "components";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: "#FFFFFF",
    padding: "100px 0",

    [breakpoints.down("xs")]: {
      padding: "70px 40px",
    },
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontWeight: 700,
    fontSize: "48px",
    lineHeight: "120.5%",
    color: "#202F9A",

    [breakpoints.down("xs")]: {
      fontSize: "35px",
      lineHeight: "38.5px",
    },
  },
  content: {
    fontFamily: "Museo Sans",
    fontSize: "14px",
    lineHeight: "150%",
    color: "#636060",

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
    },
  },

  horizon: {
    background:
      "linear-gradient(-90deg, rgba(115, 214, 241, 0) -5.46%, #73D6F1 101.08%)",
    borderRadius: "10px",
    border: "none",
    height: "3px",
    margin: "30px 0",

    [breakpoints.down("xs")]: {
      margin: "20px 0",
    },
  },
}));

const PressKitSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const [horizonWidth, setHorizonWidth] = useState(0);

  useEffect(() => {
    const container = document.querySelector("#container");
    const horizon = document.querySelector("#horizon");

    if (mobile) {
      const mobileWidth =
        (document.querySelector("body")?.clientWidth ?? 0) -
        (horizon?.getBoundingClientRect().left ?? 0);
      setHorizonWidth(mobileWidth);
    } else {
      setHorizonWidth((container?.getBoundingClientRect().width ?? 0) / 2 - 100);
    }
    // eslint-disable-next-line
  }, [mobile]);

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container id="container" spacing={3}>
          <Grid item md={3} xs={12}>
            <Box className={cx(classes.title)}>Press Kit</Box>
            <hr
              id="horizon"
              className={cx(classes.horizon)}
              style={horizonWidth ? { width: horizonWidth } : {}}
            />
            {/* <Box mt="60px" /> */}
            <Box className={cx(classes.content)}>
              Please download the relevant files required through the download
              options provided.
              <br />
              <br />
              Any queries or questions, please send us an email.
            </Box>
            {mobile && <Box mt="30px" />}
          </Grid>
          <Grid item md={3} xs={12}>
            <BrandAssetBox
              title={!mobile ? `Press\nKit` : `Press Kit`}
              content={`Contains a full collectiion of our brand logo assets.`}
              button={{
                label: "DOWNLOAD PRESS KIT",
              }}
            />
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default PressKitSection;
