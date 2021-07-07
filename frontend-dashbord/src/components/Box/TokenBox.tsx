import React from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette }) => ({
  label: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 600,
    fontSize: "13px",
    lineHeight: "100%",
    color: palette.type === "light" ? palette.text.primary : palette.text.hint,
  },

  body: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
  },

  amount: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 600,
    fontSize: "18px",
    lineHeight: "100%",
    color: palette.secondary.main,
  },

  other: {
    display: "flex",
    alignItems: "center",

    "& > div": {
      margin: "0 10px",
    },
  },

  maxButton: {
    background:
      "linear-gradient(180deg, #73D6F1 0%, #5F72FF 99.99%, #2F3DA0 100%)",
    borderRadius: "5px",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "12px",
    lineHeight: "100%",
    textAlign: "center",
    color: palette.common.white,
    padding: "5px 10px",
    cursor: "pointer",

    "&:hover": {
      color: "#0C1347",
    },
  },

  token: {
    display: "flex",
    alignItems: "center",
  },

  tokenIcon: {
    background: palette.common.white,
    borderRadius: "50%",
    padding: "10px",
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
    marginRight: "10px",

    "& img": {
      width: "30px",
      height: "30px",
    },
  },

  tokenName: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    lineHeight: "100%",
    color: palette.secondary.main,

    "& div:first-child": {
      fontSize: "17px",
      fontWeight: 500,
    },

    "& div:last-child": {
      fontSize: "12px",
      fontWeight: 500,
    },
  },
}));

export interface OverViewBoxProps {
  label: string;
  amount: number;
  token: any;
  onMaxAmount: any;
  style?: object;
  className?: string;
}

const TokenBox: React.FC<OverViewBoxProps> = ({
  label,
  amount,
  token,
  onMaxAmount,
  style={},
  className
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={className} style={style}>
      <Box className={cx(classes.label)}>
        {label}&nbsp;(${(amount * 1.23).toFixed(2)})
      </Box>
      <Box className={cx(classes.body)}>
        <Box className={cx(classes.amount)}>{amount}</Box>
        <Box className={cx(classes.other)}>
          <Box className={cx(classes.maxButton)} onClick={onMaxAmount}>MAX</Box>
          <Box className={cx(classes.token)}>
            <Box className={cx(classes.tokenIcon)}>
              <img src={token.src} alt="token icon" />
            </Box>
            <Box className={cx(classes.tokenName)}>
              <Box>{token.name}</Box>
              <Box>{token.subName}</Box>
            </Box>
          </Box>
        </Box>
      </Box>
    </Box>
  );
};

export default TokenBox;
