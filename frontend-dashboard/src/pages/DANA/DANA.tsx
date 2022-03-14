import React from "react"
import { Box, Fade, Grid, Link, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"
import { PoolRatePerDANAList, WeeklyFeeList, depositePools } from "data"
import { CheckGroup } from "components/Button"

const useStyles = makeStyles(({ palette }) => ({
  panel: {
    borderRadius: "10px",
    background:
      palette.type === "light" ? palette.common.white : palette.common.black,
    padding: "50px",
    filter: "drop-shadow(2px 2px 10px rgba(0, 0, 0, 0.1))",
    height: "100%",
  },

  displayBox: {
    borderRadius: "5px",
    padding: "30px",
    background: palette.info.main,
    color: palette.secondary.main,
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "13px",
    lineHeight: "100%",
  },

  displayText: {
    "fontFamily": "Museo Sans",
    "fontStyle": "normal",
    "fontWeight": 100,
    "fontSize": "14px",
    "lineHeight": "100%",
    "color": palette.secondary.main,
    "whiteSpace": "pre-line",

    // TODO
    "& > span": {
      fontWeight: 900,
    },
  },

  viewGuideBtn: {
    "background": palette.primary.light,
    "padding": "10px 20px",
    "borderRadius": "5px",
    "color": palette.common.white,
    "fontFamily": "Museo Sans",
    "fontStyle": "normal",
    "fontWeight": 600,
    "fontSize": "13px",
    "lineHeight": "100%",
    "width": "fit-content",

    "&:hover": {
      boxShadow:
        "0px 2px 4px -1px rgb(0 0 0 / 20%), 0px 4px 5px 0px rgb(0 0 0 / 14%), 0px 1px 10px 0px rgb(0 0 0 / 12%)",
    },
  },

  alertBox: {
    background:
      palette.type === "light" ? palette.secondary.dark : palette.info.main,
    borderRadius: "10px",
    padding: "20px",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 600,
    fontSize: "14px",
    lineHeight: "150%",
    color: palette.common.white,
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "34px",
    lineHeight: "51px",
    color: palette.secondary.main,
  },

  listHeader: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "16px",
    lineHeight: "24px",
    color: palette.secondary.main,
    textAlign: "center",
  },

  listContent: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 100,
    fontSize: "12px",
    lineHeight: "30px",
    color: palette.secondary.main,
    textAlign: "center",
  },
}))

const DANA: React.FC = () => {
  const { breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const userTheme: Theme.Theme = useUserTheme()
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

  return (
    <Fade in={true}>
      <Grid container spacing={4}>
        <Grid item xs={12} sm={6}>
          <Box component="dl" className={cx(classes.panel)} grid-gap={"40px"}>
            <Box className={cx(classes.displayBox)}>
              <Box>1 DANA locked for 4 years = 1exDANA</Box>
              {/* TODO: use padding/gap, not breaks …and maybe change to a more descriptive elements */}
              <br />
              <Box>1 DANA locked for 3 years = 0. 75exDANA</Box>
              <br />
              <Box>1 DANA locked for 2 years = 0.50exDANA</Box>
              <br />
              <Box>1 DANA locked for 1 year = 0.25exDANA</Box>
              <br />
              <br />
              <Link href="#" underline="none">
                <Box className={cx(classes.viewGuideBtn)}>
                  VIEW exDANA GUIDE
                </Box>
              </Link>
            </Box>

            <dt>exDANA holder/LP ratio</dt>
            <dd>22.56</dd>

            <aside className={cx(classes.alertBox)}>
              Having locked 1$ in DANA for 4 years is equal to having provided
              22.56$ as an LP
            </aside>

            <dt>exDANA holder APY</dt>
            <dd>8.85%</dd>

            <dt>Yearly fee earnings per 1 exDANA</dt>
            <dd>0.16$</dd>

            <dt>exDANA balance</dt>
            <dd>0 Stake DANA</dd>

            <dt>Average daily earnings</dt>
            <dd>$95,369.23</dd>

            <dt>Last weekly earnings</dt>
            <dd>$667,584.61</dd>

            <dt>Total DANA Locked</dt>
            <dd>242,785,542.45</dd>

            <dt>Total exDANA</dt>
            <dd>222,832,093.00</dd>

            <dt>Next Distribution</dt>
            {/* TODO: Intl.DateFormate */}
            <dd>Sat, 10 Jul 2021 04:36:06 GMT</dd>

            <div style={{ fontWeight: 700 }}>Stake your DANA</div>
            <div>Guide to staking DANA</div>
          </Box>
        </Grid>
        <Grid item xs={12} sm={6}>
          <Box className={cx(classes.panel)}>
            <Box className={cx(classes.alertBox)}>
              Depositing into multiple pools lets you have a 2.5x boost for a
              bigger $ amount per 1 exDANA
            </Box>
            <Box mt="40px"></Box>

            <CheckGroup list={depositePools} />

            <Box mt="30px"></Box>

            <Box className={cx(classes.displayText)}>
              Max $ per 1 exDANA to have 2.5x boost: 0$
            </Box>
          </Box>
        </Grid>
        <Grid item xs={12}>
          <Box className={cx(classes.panel)}>
            <Box className={cx(classes.title)}>Use DANA</Box>

            <Box mt="30px"></Box>

            <Grid container>
              <Grid item xs={6}>
                <Box className={cx(classes.listHeader)}>Pool</Box>
              </Grid>
              <Grid item xs={6}>
                <Box className={cx(classes.listHeader)}>
                  Max $ per DANA to have 2.5x boost
                </Box>
              </Grid>
            </Grid>

            <Box mt="30px"></Box>

            {PoolRatePerDANAList.map((item, index) => (
              <Grid container key={index}>
                <Grid item xs={6}>
                  <Box className={cx(classes.listContent)}>{item.pool}</Box>
                </Grid>
                <Grid item xs={6}>
                  <Box className={cx(classes.listContent)}>
                    {item.rate + "$"}
                  </Box>
                </Grid>
              </Grid>
            ))}
          </Box>
        </Grid>
        <Grid item xs={12}>
          <Box className={cx(classes.panel)}>
            <Box className={cx(classes.title)}>Weekly Fees</Box>

            <Box mt="30px"></Box>

            <Grid container>
              <Grid item xs={6}>
                <Box className={cx(classes.listHeader)}>Week Start</Box>
              </Grid>
              <Grid item xs={6}>
                <Box className={cx(classes.listHeader)}>Fees</Box>
              </Grid>
            </Grid>

            <Box mt="30px"></Box>

            {WeeklyFeeList.map((item, index) => (
              <Grid container key={index}>
                <Grid item xs={6}>
                  <Box className={cx(classes.listContent)}>{item.week}</Box>
                </Grid>
                <Grid item xs={6}>
                  <Box className={cx(classes.listContent)}>
                    {"$" + item.fee}
                  </Box>
                </Grid>
              </Grid>
            ))}
          </Box>
        </Grid>
      </Grid>
    </Fade>
  )
}

export default DANA
