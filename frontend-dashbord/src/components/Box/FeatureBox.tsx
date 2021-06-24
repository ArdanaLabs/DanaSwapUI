import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

export interface FeatureBoxProps {
  image?: any;
  title?: string;
  content?: string;
  custom_style?: object;
}

const useStyles = makeStyles(({ palette }) => ({
  bg: {
    position: "relative",
    borderRadius: "10px",
    flex: 2,
    marginBottom: "30px",
    cursor: "pointer",
    transition: "transform .3s",

    "&:hover": {
      transform: "scale(1.05)",
    },
  },

  title: {
    color: "white",
    whiteSpace: "pre-line",
    fontSize: "36px",
    fontWeight: 900,
  },
  mobile_title: {
    color: "white",
    whiteSpace: "unset",
    fontSize: "24px",
    fontWeight: 900,
  },

  content: {
    color: "white",
    fontSize: "18px",
    lineHeight: "21px",
    fontWeight: 300,
  },
  mobile_content: {
    color: "white",
    fontSize: "16px",
    lineHeight: "21px",
    fontWeight: 300,
  },
}));

const FeatureBox: React.FC<FeatureBoxProps> = ({
  image,
  title,
  content,
  custom_style,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)} style={custom_style}>
      <Box mt="-50px">
        <img width={!mobile ? "170px" : "100px"} src={image} alt="title" />
      </Box>

      <Box className={cx(!mobile ? classes.title : classes.mobile_title)}>
        {title}
      </Box>

      <Box mt="20px"></Box>

      <Box className={cx(!mobile ? classes.content : classes.mobile_content)}>
        {content}
      </Box>
    </Box>
  );
};

export default FeatureBox;
