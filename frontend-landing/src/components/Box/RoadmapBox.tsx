import React from "react";
import cx from "classnames";
import { Box, makeStyles, useMediaQuery, useTheme } from "@material-ui/core";
import { useIsDarkMode } from "state/user/hooks";
import { GradientBox } from "components/Box";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    padding: "20px",
    width: "100%",

    "& .title": {
      position: "relative",
      padding: "20px 0px 20px 0px",

      "& span": {
        fontFamily: "Brandon Grotesque",
        fontWeight: 700,
        fontSize: "20px",
        lineHeight: "100%",
        color: palette.common.white,

        [breakpoints.down("xs")]: {
          fontSize: "14px",
        },
      },
    },

    "& .plan": {
      background: palette.background.paper,
      borderRadius: "10px",
      padding: "10px",
      margin: "7px 0px",
      textAlign: "center",
      width: "100%",

      fontFamily: "Brandon Grotesque",
      fontWeight: 700,
      fontSize: "15px",
      lineHeight: "100%",
      color: palette.common.white,
    },
  },

  [breakpoints.down("xs")]: { padding: "10px" },
}));

interface Props {
  term: string;
  plans: string[];
}

const RoadmapBox: React.FC<Props> = ({ term, plans }) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)} display="flex" flexDirection={"column"}>
      <Box className="title" textAlign={"center"}>
        <GradientBox width={!mobile ? 100 : 75} height={!mobile ? 100 : 75}>
          <span>{term}</span>
        </GradientBox>
      </Box>
      {plans.map((plan) => (
        <Box key={plan} className={"plan"}>
          {plan}
        </Box>
      ))}
    </Box>
  );
};

export default RoadmapBox;
