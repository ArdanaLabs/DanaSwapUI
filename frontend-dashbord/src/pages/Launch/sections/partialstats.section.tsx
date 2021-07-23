import React, { useState } from "react";
import { Box, Container, Grid, Fade, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import cx from "classnames";
import { StatBox } from "components/Box";

import IMG_TVL from "assets/icons/tvl.png";
import IMG_Worth from "assets/icons/worth.png";
import IMG_Ratio from "assets/icons/ratio.png";
import VerticalCarousel from "components/Carousel";

const useStyles = makeStyles(({ palette }) => ({
  root: {
    background: "linear-gradient(180deg, #01062F 14.06%, #172271 100%)",
    position: "fixed",
    width: "100vw",
    height: "100vh",
    top: 0,
    left: 0,

    display: "flex",
    alignItems: "center",
  },

  carousel: {
    display: "flex",
    flexDirection: "column",
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    lineHeight: "120.5%",
    cursor: "pointer",

    "& > div": {
      margin: "30px 0px",
    },
    // "& > div:first-child": {
    //   background: "-webkit-linear-gradient(-90deg, #FFFFFF, #ffffff00)",
    //   "-webkit-background-clip": "text",
    //   "-webkit-text-fill-color": "transparent",
    // },
    // "& > div:last-child": {
    //   background: "-webkit-linear-gradient(90deg, #FFFFFF, #ffffff00)",
    //   "-webkit-background-clip": "text",
    //   "-webkit-text-fill-color": "transparent",
    // }
  },
  inactive: {
    fontSize: "30px",
    fontWeight: 300,
    lineHeight: "120.5%",
  },
  active: {
    fontSize: "45px",
    fontWeight: 900,
    lineHeight: "120.5%",
  },
}));

const PartialStatsSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const dark = useIsDarkMode();
  const classes = useStyles({ dark, mobile });

  const [activeIndex, setActiveIndex] = useState(0);

  return (
    <Box className={cx(classes.root)}>
      <Container style={{ marginTop: "50px" }}>
        <Grid container>
          <Grid item xs={12} sm={6} md={4} className={cx(classes.carousel)}>
            <VerticalCarousel
              activeIndex={activeIndex}
              setActiveIndex={setActiveIndex}
              data={["DANASWAP", `ARDANA\nSTABLECOINS`, "MY DASHBOARD"]}
            />
          </Grid>

          {/* DANASWAP */}
          {activeIndex === 0 && (
            <Grid
              container
              item
              xs={12}
              sm={6}
              md={8}
              spacing={3}
              alignContent="flex-end"
            >
              <Fade in={activeIndex === 0}>
                <Grid item xs={4}>
                  <StatBox
                    image={IMG_TVL}
                    title="TOTAL VALUE LOCKED"
                    content="$1,234,567"
                    delay={0}
                  />
                </Grid>
              </Fade>
              <Fade
                in={activeIndex === 0}
                style={{ transitionDelay: activeIndex === 0 ? "500ms" : "0ms" }}
              >
                <Grid item xs={4}>
                  <StatBox
                    image={IMG_Worth}
                    title={`WORTH OF STABLECOINS\nIN CIRCULATION`}
                    content="$998,654"
                    delay={500}
                  />
                </Grid>
              </Fade>
              <Fade
                in={activeIndex === 0}
                style={{
                  transitionDelay: activeIndex === 0 ? "1000ms" : "0ms",
                }}
              >
                <Grid item xs={4}>
                  <StatBox
                    image={IMG_Ratio}
                    title="TOTAL COLLATERAL-TO-LOAN RATIO"
                    content="1:1"
                    delay={1000}
                  />
                </Grid>
              </Fade>
            </Grid>
          )}
          
          {/* DANASWAP */}
          {activeIndex === 1 && (
            <Grid
              container
              item
              xs={12}
              sm={6}
              md={8}
              spacing={3}
              alignContent="flex-end"
            >
              <Fade in={activeIndex === 1}>
                <Grid item xs={4}>
                  <StatBox
                    image={IMG_TVL}
                    title="TOTAL VALUE LOCKED"
                    content="$1,234,567"
                    delay={0}
                  />
                </Grid>
              </Fade>
              <Fade
                in={activeIndex === 1}
                style={{ transitionDelay: activeIndex === 1 ? "500ms" : "0ms" }}
              >
                <Grid item xs={4}>
                  <StatBox
                    image={IMG_Worth}
                    title={`WORTH OF STABLECOINS\nIN CIRCULATION`}
                    content="$998,654"
                    delay={500}
                  />
                </Grid>
              </Fade>
              <Fade
                in={activeIndex === 1}
                style={{
                  transitionDelay: activeIndex === 1 ? "1000ms" : "0ms",
                }}
              >
                <Grid item xs={4}>
                  <StatBox
                    image={IMG_Ratio}
                    title="TOTAL COLLATERAL-TO-LOAN RATIO"
                    content="1:1"
                    delay={1000}
                  />
                </Grid>
              </Fade>
            </Grid>
          )}
          
          {/* MY DASHBOARD */}
          {activeIndex === 2 && (
            <Grid
              container
              item
              xs={12}
              sm={6}
              md={8}
              spacing={3}
              alignContent="flex-end"
            >
              <Fade in={activeIndex === 2}>
                <Grid item xs={4}>
                  <StatBox
                    image={IMG_TVL}
                    title="TOTAL VALUE LOCKED"
                    content="$1,234,567"
                    delay={0}
                  />
                </Grid>
              </Fade>
              <Fade
                in={activeIndex === 2}
                style={{ transitionDelay: activeIndex === 2 ? "500ms" : "0ms" }}
              >
                <Grid item xs={4}>
                  <StatBox
                    image={IMG_Worth}
                    title={`WORTH OF STABLECOINS\nIN CIRCULATION`}
                    content="$998,654"
                    delay={500}
                  />
                </Grid>
              </Fade>
              <Fade
                in={activeIndex === 2}
                style={{
                  transitionDelay: activeIndex === 2 ? "1000ms" : "0ms",
                }}
              >
                <Grid item xs={4}>
                  <StatBox
                    image={IMG_Ratio}
                    title="TOTAL COLLATERAL-TO-LOAN RATIO"
                    content="1:1"
                    delay={1000}
                  />
                </Grid>
              </Fade>
            </Grid>
          )}
        </Grid>
      </Container>
    </Box>
  );
};

export default PartialStatsSection;
