import React from "react";
import { Box, Grid, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import Chart from "react-apexcharts";

import { useIsDarkMode } from "state/user/hooks";
import { ApexOptions } from "apexcharts";

const useStyles = makeStyles(({ palette }) => ({
  self: {
    background: "unset",
  },

  title: {
    color: palette.text.primary,
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "30px",
    lineHeight: "110%",
  },

  panel: {
    background: palette.type === "light" ? palette.common.white : palette.background.paper,
    borderRadius: "10px",
    padding: "20px",
    filter: palette.type === "light" ? "drop-shadow(2px 2px 10px rgba(0, 0, 0, 0.1))" : "unset",
  },

  panelFilter: {
    display: "flex",
    justifyContent: "space-between",
    color: palette.text.hint,
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "13px",
    lineHeight: "100%",
    paddingBottom: "30px",

    "& span": {
      padding: "10px",
      cursor: "pointer",
    },
  },

  panelFilterByType: {},

  panelFilterByDate: {},
}));

const ChartSection: React.FC = () => {
  const { palette, breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const options: ApexOptions = {
    chart: {
      id: "basic-bar",
      zoom: {
        enabled: false,
      },
      toolbar: {
        show: false,
      },
    },
    stroke: {
      width: 0,
      curve: "smooth",
    },
    xaxis: {
      categories: ["APR 20", "MAY 15", "JUN 02"],
      labels: {
        style: {
          colors: [palette.text.hint, palette.text.hint, palette.text.hint],
          fontSize: "13px",
          fontFamily: "Museo Sans",
          fontWeight: 500,
        },
      },
      tickPlacement: "between",
    },
    yaxis: {
      labels: {
        show: true,
        align: "left",
        style: {
          colors: [palette.secondary.main],
          fontFamily: "Museo Sans",
          fontWeight: "bold",
          fontSize: "18px",
          cssClass: "apexcharts-yaxis-label",
        },
        formatter: (value: any) => {
          return "$" + value + (value ? " M" : "");
        },
      },
    },
    fill: {
      type: "gradient",
      colors: [!dark ? "#202F9A" : "#73d6f1"],
      gradient: {
        type: "vertical", // The gradient in the horizontal direction
        gradientToColors: [!dark ? "#5F72FF" : "#73D6F1"], // The color at the end of the gradient
        opacityFrom: 1, // transparency
        opacityTo: 0.3,
        stops: [0, 1200],
      },
    },
    plotOptions: {
      bar: {
        borderRadius: 5,
      },
    },
    dataLabels: {
      enabled: false,
    },
    tooltip: {
      enabled: false,
    },
  };

  const series = [
    {
      name: "series-1",
      data: [30, 40, 45, 50, 49, 60, 70, 91, 30, 40, 45, 50, 49, 60, 70, 91],
    },
  ];

  return (
    <Box className={cx(classes.self)}>
      <Grid container spacing={3}>
        <Grid item sm={12} md={6} style={{width: "100%"}}>
          <Box className={cx(classes.title)}>Volume</Box>
          <Box mt="20px" />
          <Box className={cx(classes.panel)}>
            <Box className={cx(classes.panelFilter)}>
              <Box className={cx(classes.panelFilterByType)}>
                <Box component="span">TOTAL</Box>
                <Box component="span">SWAP</Box>
                <Box component="span">ADD</Box>
                <Box component="span">WITHDRAW</Box>
              </Box>
              <Box className={cx(classes.panelFilterByDate)}>
                <Box component="span">WEEK</Box>
                <Box component="span">ALL</Box>
              </Box>
            </Box>
            <Box>
              <Chart
                options={options}
                series={series}
                type="bar"
                width="100%"
              />
            </Box>
          </Box>
        </Grid>
        <Grid item sm={12} md={6} style={{width: "100%"}}>
          <Box className={cx(classes.title)}>Liquidity</Box>
          <Box mt="20px" />
          <Box className={cx(classes.panel)}>
            <Box className={cx(classes.panelFilter)}>
              <Box className={cx(classes.panelFilterByType)}>
                <Box component="span">LIQUIDITY</Box>
                <Box component="span">LP EARNING</Box>
                <Box component="span">BOND EARNING</Box>
                <Box component="span">$RUNE PRICE</Box>
              </Box>
              <Box className={cx(classes.panelFilterByDate)}>
                <Box component="span">WEEK</Box>
                <Box component="span">ALL</Box>
              </Box>
            </Box>
            <Box>
              <Chart
                options={options}
                series={series}
                type="area"
                width="100%"
              />
            </Box>
          </Box>
        </Grid>
      </Grid>
    </Box>
  );
};

export default ChartSection;
