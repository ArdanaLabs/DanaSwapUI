import React, { useEffect, useState } from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { BrandAssetBox } from "components";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: "#F6F6F6",
    padding: "100px 0",
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontWeight: 700,
    fontSize: "48px",
    lineHeight: "120.5%",
    color: "#202F9A",
  },
  content: {
    fontFamily: "Museo Sans",
    fontSize: "14px",
    lineHeight: "150%",
    color: "#202F9A",
  },

  horizen: {
    background:
      "linear-gradient(-90deg, rgba(115, 214, 241, 0) -5.46%, #73D6F1 101.08%)",
    borderRadius: "10px",
    border: "none",
    height: "3px",
    margin: "30px 0",
  },
}));

const GuideLinesSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const [horizenWidth, setHorizenWidth] = useState(0);

  useEffect(() => {
    const container = document.querySelector("#container");
    setHorizenWidth((container?.clientWidth ?? 0) - 100);
    // eslint-disable-next-line
  }, [mobile]);

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container id="container" spacing={3}>
          <Grid item md={3} xs={12}>
            <Box className={cx(classes.title)}>Guidelines</Box>
            <hr
              className={cx(classes.horizen)}
              style={horizenWidth ? { width: horizenWidth } : {}}
            />
            {/* <Box mt="60px" /> */}
            <Box className={cx(classes.content)}>
              We also have separate brand guidelines documents for both our ASPA
              and dUSD brands.
              <br />
              <br />
              Please ensure to utilise these accordingly.
              <br />
              Any queries or questions, please send us an email.
            </Box>
          </Grid>
          <Grid item md={3} xs={12}>
            <BrandAssetBox
              title={`Ardana Brand\nGuidelines`}
              content={`A full document demonstrating how our brand is properly used.`}
              button={{
                label: "DOWNLOAD BRAND GUIDELINES",
              }}
            />
          </Grid>
          <Grid item md={3} xs={12}>
            <BrandAssetBox
              title={`dUSD Brand\nGuidelines`}
              content={`A full document demonstrating the proper usage of our dUSD brand.`}
              button={{
                label: "DOWNLOAD BRAND GUIDELINES",
              }}
            />
          </Grid>
          <Grid item md={3} xs={12}>
            <BrandAssetBox
              title={`ASPA Brand\nGuidelines`}
              content={`A full document demonstrating the proper usage of our ASPA brand.`}
              button={{
                label: "DOWNLOAD BRAND GUIDELINES",
              }}
            />
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default GuideLinesSection;
