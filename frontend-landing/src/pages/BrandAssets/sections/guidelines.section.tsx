import React, { useEffect, useState } from "react"
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { BrandAssetBox } from "components"

import BRAND_ARDANA from "assets/brands/Ardana.png"
import BRAND_DUSD from "assets/brands/dUSD.png"
import BRAND_ASPA from "assets/brands/ASPA.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: "#F6F6F6",
    padding: "100px 0",

    [breakpoints.down("xs")]: {
      padding: "70px 40px",
    },
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontWeight: 700,
    fontSize: "45px",
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
}))

const GuideLinesSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const [horizonWidth, setHorizonWidth] = useState(0)

  useEffect(() => {
    const container = document.querySelector("#container")
    const horizon = document.querySelector("#horizon")

    if (mobile) {
      const mobileWidth =
        (document.querySelector("body")?.clientWidth ?? 0) -
        (horizon?.getBoundingClientRect().left ?? 0)
      setHorizonWidth(mobileWidth)
    } else {
      setHorizonWidth((container?.getBoundingClientRect().width ?? 0) - 100)
    }
    // eslint-disable-next-line
  }, [mobile])

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container id="container" spacing={3}>
          <Grid item md={3} xs={12}>
            <Box className={cx(classes.title)}>Guidelines</Box>
            <hr
              className={cx(classes.horizon)}
              style={horizonWidth ? { width: horizonWidth } : {}}
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
            {mobile && <Box mt="30px" />}
          </Grid>
          <Grid item md={3} xs={12}>
            <BrandAssetBox
              brand={BRAND_ARDANA}
              title={
                !mobile
                  ? `Ardana Brand\nGuidelines`
                  : `Ardana\nBrand Guidelines`
              }
              content={`A full document demonstrating how our brand is properly used.`}
              button={{
                label: "DOWNLOAD BRAND GUIDELINES",
                link: "https://docsend.com/view/k4xzgh5qafknt8jz",
              }}
            />
          </Grid>
          <Grid item md={3} xs={12}>
            <BrandAssetBox
              brand={BRAND_DUSD}
              title={
                !mobile ? `dUSD Brand\nGuidelines` : `dUSD\nBrand Guidelines`
              }
              content={`A full document demonstrating the proper usage of our dUSD brand.`}
              button={{
                label: "DOWNLOAD BRAND GUIDELINES",
                link: "https://docsend.com/view/qywv6rjwfhvaaa2n",
              }}
            />
          </Grid>
          <Grid item md={3} xs={12}>
            <BrandAssetBox
              brand={BRAND_ASPA}
              title={
                !mobile ? `ASPA Brand\nGuidelines` : `ASPA\nBrand Guidelines`
              }
              content={`A full document demonstrating the proper usage of our ASPA brand.`}
              button={{
                label: "DOWNLOAD BRAND GUIDELINES",
              }}
            />
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default GuideLinesSection