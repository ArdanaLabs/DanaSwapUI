import React, { useState } from "react";
import cx from "classnames";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import { TokenSelectorDialog } from "components/Dialog";
import { Input } from "components/Input";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  label: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 600,
    fontSize: "11px",
    lineHeight: "100%",
    color: palette.type === "light" ? palette.text.primary : palette.text.hint,
  },

  body: {
    display: "flex",
    alignItems: "center",
  },

  amount: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 600,
    fontSize: "16px",
    lineHeight: "100%",
    color: `${palette.secondary.main} !important`,
    background: "transparent",
  },

  other: {
    display: "flex",
    alignItems: "center",

    "& > div": {
      margin: "0 10px",
      width: "100px",
    },
  },

  maxButton: {
    background:
      "linear-gradient(180deg, #73D6F1 0%, #5F72FF 99.99%, #2F3DA0 100%)",
    borderRadius: "5px",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "10px",
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
    cursor: "pointer",
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
  noTokenIcon: {
    background: palette.common.white,
    borderRadius: "50%",
    padding: "10px",
    marginRight: "10px",
    width: "30px",
    height: "30px",
  },

  tokenName: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    lineHeight: "100%",
    color: palette.secondary.main,
    flexGrow: 2,

    "& div:first-child": {
      fontSize: "15px",
      fontWeight: 500,
    },

    "& div:last-child": {
      fontSize: "10px",
      fontWeight: 500,
    },
  },

  filterText: {
    background: palette.common.white,
    fontSize: "10px",
    fontWeight: 500,
    lineHeight: "100%",
    width: "100%",
    padding: "15px 20px",
    borderRadius: "10px",
    color: palette.type === "light" ? palette.primary.main : palette.text.hint,

    "&::placeholder": {
      color:
        palette.type === "light" ? palette.primary.main : palette.text.hint,
    },
  },

  filterType: {
    background: "transparent",
    padding: "5px 10px",
    fontSize: "10px",
    lineHeight: "100%",
    margin: "20px 10px 20px 0px",
    border: `1px solid ${palette.secondary.main}`,
    color: palette.text.secondary,
  },

  active: {
    background: palette.primary.light,
    border: "unset",
    color: palette.common.white,
  },

  menuItem: {
    borderBottom: "1px solid white",
    display: "flex",
    justifyContent: "space-between",
  },
}));

export interface TokenBoxProps {
  label: string;
  amount: number;
  token: any;
  onAmountChange: (amount: number) => void;
  style?: object;
  className?: string;
  handleTokenSelect: (token: any) => void;
}

const TokenBox: React.FC<TokenBoxProps> = ({
  label,
  amount,
  token,
  onAmountChange,
  style = {},
  className,
  handleTokenSelect,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const [tokenSelectorDialogOpen, setTokenSelectorDialogOpen] = useState(false);

  return (
    <Box className={className} style={style}>
      <Box className={cx(classes.label)}>
        {label}&nbsp;(${(amount * 1.23).toFixed(2)})
      </Box>
      <Box className={cx(classes.body)}>
        <Box
          display="flex"
          justifyContent="space-between"
          alignItems="center"
          width="100%"
        >
          <Input
            type="number"
            value={amount}
            max={token?.quantity ?? 0}
            className={cx(classes.amount)}
            onChange={(e: any) => onAmountChange(e.target.value)}
          />
          <Box
            id="max_button"
            className={cx(classes.maxButton)}
            onClick={() => onAmountChange(token?.quantity ?? 0)}
          >
            MAX
          </Box>
        </Box>
        <Box className={cx(classes.other)}>
          <Box
            className={cx(classes.token)}
            onClick={() => setTokenSelectorDialogOpen(true)}
          >
            {!token.logo && <Box className={cx(classes.noTokenIcon)}></Box>}
            {token.logo && (
              <Box className={cx(classes.tokenIcon)}>
                {token.logo && <img src={token.logo} alt="token icon" />}
              </Box>
            )}
            {token.unit && (
              <Box className={cx(classes.tokenName)}>
                <Box>{token.unit}</Box>
                <Box>exDANA</Box>
              </Box>
            )}
          </Box>
        </Box>
      </Box>
      <TokenSelectorDialog
        open={tokenSelectorDialogOpen}
        handleClose={() => setTokenSelectorDialogOpen(false)}
        handleTokenSelect={handleTokenSelect}
      />
    </Box>
  );
};

export default TokenBox;
