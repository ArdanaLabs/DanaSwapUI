import React, { useState } from "react";
import cx from "classnames";
import { Box, List, ListItem, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

import { TokenList } from "data";
import { Dialog, DialogTitle } from "components/Dialog";
import { Button } from "components/Button";
import { SearchInput } from "components/Input";

const FILTER_ALL = 0;
const FILTER_NATIVE = 1;
const FILTER_ERC20 = 2;
const FILTER_BEP2 = 3;

const useStyles = makeStyles(({ palette, breakpoints }) => ({
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

  filterText: {
    background: palette.common.white,
    fontSize: "12px",
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
    fontSize: "12px",
    lineHeight: "100%",
    margin: "20px 10px 20px 0px",
    border: `1px solid ${palette.secondary.main}`,
  },

  active: {
    background: palette.primary.light,
    border: "unset",
  },

  menuItem: {
    borderBottom: "1px solid white",
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
  const [openTokenDlg, setOpenTokenDlg] = useState(false);

  const [filter, setFilter] = useState({
    text: "",
    type: 0,
  });

  const onFilterChange = (event: any) => {
    setFilter({ ...filter, ...event });
  };

  const handleClickOpen = () => {
    setOpenTokenDlg(true);
  };
  const handleClose = () => {
    setOpenTokenDlg(false);
  };

  const handleMenuItemClick = (token: any) => {
    handleTokenSelect(token);
    setOpenTokenDlg(false);
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
          <Box className={cx(classes.token)} onClick={handleClickOpen}>
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
          <Dialog
            onClose={handleClose}
            aria-labelledby="simple-dialog-title"
            open={openTokenDlg}
          >
            <DialogTitle id="customized-dialog-title" onClose={handleClose}>
              SELECT ASSET
            </DialogTitle>

            <Box>
              <Button
                variant="contained"
                onClick={() => {
                  onFilterChange({ type: FILTER_ALL });
                }}
                className={cx(classes.filterType, {
                  [classes.active]: filter.type === FILTER_ALL,
                })}
              >
                All
              </Button>
              <Button
                variant="contained"
                onClick={() => {
                  onFilterChange({ type: FILTER_NATIVE });
                }}
                className={cx(classes.filterType, {
                  [classes.active]: filter.type === FILTER_NATIVE,
                })}
              >
                NATIVE
              </Button>
              <Button
                variant="contained"
                onClick={() => {
                  onFilterChange({ type: FILTER_ERC20 });
                }}
                className={cx(classes.filterType, {
                  [classes.active]: filter.type === FILTER_ERC20,
                })}
              >
                ERC20
              </Button>
              <Button
                variant="contained"
                onClick={() => {
                  onFilterChange({ type: FILTER_BEP2 });
                }}
                className={cx(classes.filterType, {
                  [classes.active]: filter.type === FILTER_BEP2,
                })}
              >
                BEP2
              </Button>
            </Box>

            <SearchInput
              className={cx(classes.filterText)}
              value={filter.text}
              placeholder="SEARCH..."
              isIcon={true}
              onChange={(e: any) => {
                onFilterChange({ text: e.target.value });
              }}
            />

            <List>
              {TokenList.map((item, index) => (
                <ListItem
                  button
                  className={cx(classes.menuItem)}
                  onClick={() => handleMenuItemClick(item)}
                  key={index + 1}
                >
                  <Box className={cx(classes.tokenIcon)}>
                    <img src={item.src} alt="token icon" />
                  </Box>
                  <Box className={cx(classes.tokenName)}>
                    <Box>{item.name}</Box>
                    <Box flexGrow={2}>{item.desc}</Box>
                    <Box>asdf</Box>
                  </Box>
                </ListItem>
              ))}
            </List>
          </Dialog>
        </Box>
      </Box>
    </Box>
  );
};

export default TokenBox;
