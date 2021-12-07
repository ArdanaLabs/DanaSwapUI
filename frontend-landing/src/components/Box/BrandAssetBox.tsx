import React from "react";
import cx from "classnames";
import { Box, Link, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: "#1C2679",
    borderRadius: "20px",
    padding: "40px",
    [breakpoints.down("xs")]: {
      padding: "30px",
    },
  },
  brand: {
    marginBottom: "10px",
    "& > img": {
      height: "45px",
    },
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontWeight: 700,
    fontSize: "30px",
    lineHeight: "110%",
    color: "#FFFFFF",
    whiteSpace: "pre-line",

    [breakpoints.down("xs")]: {
      fontSize: "25px",
      lineHeight: "27.5px",
    },
  },
  content: {
    fontFamily: "Museo Sans",
    fontSize: "15px",
    lineHeight: "22px",
    color: "#FFFFFF",
    whiteSpace: "pre-line",

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "19.2px",
    },
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
  brand?: string;
  title: string;
  content: string;
  button: {
    label: string;
    link?: string;
  };
}

const BrandAsset: React.FC<BrandAssetProps> = ({
  brand = null,
  title,
  content,
  button,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      {brand && (
        <Box className={cx(classes.brand)}>
          <img src={brand} alt="brand" />
        </Box>
      )}
      <Box className={cx(classes.title)}>{title}</Box>
      <Box mt="20px" />
      <Box className={cx(classes.content)}>{content}</Box>
      <Box mt="20px" />
      {button.link && (
        <Link href={button.link} target="_blank" underline="none">
          <Box className={cx(classes.button)}>{button.label}</Box>
        </Link>
      )}
      {!button.link && <Box className={cx(classes.button)}>{button.label}</Box>}
    </Box>
  );
};

export default BrandAsset;
