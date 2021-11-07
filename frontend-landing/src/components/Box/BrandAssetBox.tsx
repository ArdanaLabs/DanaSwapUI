import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: "#1C2679",
    borderRadius: "20px",
    padding: "40px",
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontWeight: 700,
    fontSize: "30px",
    lineHeight: "110%",
    color: "#FFFFFF",
    whiteSpace: "pre-line",
  },
  content: {
    fontFamily: "Museo Sans",
    fontSize: "15px",
    lineHeight: "22px",
    color: "#FFFFFF",
  },
  button: {
    background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
    borderRadius: "20px",
    fontFamily: "Museo Sans",
    fontWeight: 700,
    fontSize: "11px",
    lineHeight: "100%",
    color: "#FFFFFF",
    textAlign: "center",
    padding: "10px 0",
    width: "100%",
    cursor: "pointer",
  },
}));

interface BrandAssetProps {
  title: string;
  content: string;
  button: {
    label: string;
    link?: string;
  };
}

const BrandAsset: React.FC<BrandAssetProps> = ({ title, content, button }) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <Box className={cx(classes.title)}>{title}</Box>
      <Box mt="20px" />
      <Box className={cx(classes.content)}>{content}</Box>
      <Box mt="20px" />
      {/* <Link href={button.link}> */}
      <Box className={cx(classes.button)}>{button.label}</Box>
      {/* </Link> */}
    </Box>
  );
};

export default BrandAsset;
