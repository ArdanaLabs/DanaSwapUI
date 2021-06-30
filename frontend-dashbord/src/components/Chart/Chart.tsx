import React from "react";
import Chart from "react-apexcharts";
import { ApexOptions } from "apexcharts";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette }) => ({
  self: {
    margin: "0px 10px",
  },
}));

export interface OverViewBoxProps {
  options?: ApexOptions;
  series?: Array<any>;
  type?: any;
}

const OverViewBox: React.FC<OverViewBoxProps> = ({ type, series, options }) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });


  return (
    <Box className={cx(classes.self)}>
      <Chart
        options={options}
        series={series}
        type={type}
        width="100%"
      />
    </Box>
  );
};

export default OverViewBox;
