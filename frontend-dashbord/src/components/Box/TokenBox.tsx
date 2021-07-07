import React, { useState } from "react";
import cx from "classnames";
import { Box, MenuItem, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import { Menu } from "components/Menu";

import { TokenList } from "data";

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
  handleTokenSelect: any;
}

const TokenBox: React.FC<OverViewBoxProps> = ({
  label,
  amount,
  token,
  onMaxAmount,
  handleTokenSelect,
  style = {},
  className,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const [anchorEl, setAnchorEl] = useState<null | HTMLElement>(null);

  const handleClickListItem = (event: React.MouseEvent<HTMLElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const handleMenuItemClick = (
    event: React.MouseEvent<HTMLElement>,
    token: any
  ) => {
    setAnchorEl(null);
    handleTokenSelect(token);
  };

  const handleClose = () => {
    setAnchorEl(null);
  };

  return (
    <Box className={className} style={style}>
      <Box className={cx(classes.label)}>
        {label}&nbsp;(${(amount * 1.23).toFixed(2)})
      </Box>
      <Box className={cx(classes.body)}>
        <Box className={cx(classes.amount)}>{amount}</Box>
        <Box className={cx(classes.other)}>
          <Box className={cx(classes.maxButton)} onClick={onMaxAmount}>
            MAX
          </Box>
          <Box className={cx(classes.token)} onClick={handleClickListItem}>
            {!token.src && <Box className={cx(classes.noTokenIcon)}></Box>}
            {token.src && (
              <Box className={cx(classes.tokenIcon)}>
                {token.src && <img src={token.src} alt="token icon" />}
              </Box>
            )}
            {token.name && (
              <Box className={cx(classes.tokenName)}>
                <Box>{token.name}</Box>
                <Box>{token.desc}</Box>
              </Box>
            )}
          </Box>
          <Menu
            id="lock-menu"
            anchorEl={anchorEl}
            keepMounted
            open={Boolean(anchorEl)}
            onClose={handleClose}
            elevation={0}
            getContentAnchorEl={null}
            anchorOrigin={{
              vertical: "bottom",
              horizontal: "right",
            }}
            transformOrigin={{
              vertical: "top",
              horizontal: "center",
            }}
          >
            <MenuItem key={0} disabled={true}>
              Select Token
            </MenuItem>
            {TokenList.map((item, index) => (
              <MenuItem
                key={index + 1}
                selected={token.name && item.name === token.name}
                onClick={(event) => handleMenuItemClick(event, item)}
              >
                <Box className={cx(classes.tokenIcon)}>
                  <img src={item.src} alt="token icon" />
                </Box>
                <Box>{item.name}</Box>
              </MenuItem>
            ))}
          </Menu>
        </Box>
      </Box>
    </Box>
  );
};

export default TokenBox;
