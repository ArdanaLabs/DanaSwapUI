import React, { useState } from "react";
import cx from "classnames";
import { Box, List, ListItem, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";

import { TokenList } from "data";
import { Dialog, DialogTitle } from "components/Dialog";
import { Button } from "components/Button";
import { SearchInput } from "components/Input";
import { Currency } from "state/wallet/actions";

const FILTER_ALL = 0;
const FILTER_NATIVE = 1;
const FILTER_ERC20 = 2;
const FILTER_BEP2 = 3;

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  amount: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 600,
    fontSize: "16px",
    lineHeight: "100%",
    color: palette.secondary.main,
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

export interface TokenSelectorProps {
  open: boolean;
  handleClose: () => void;
  handleTokenSelect: (token: Currency) => void;
}

const TokenSelectorDialog: React.FC<TokenSelectorProps> = ({
  open,
  handleClose,
  handleTokenSelect,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const [filter, setFilter] = useState({
    text: "",
    type: 0,
  });

  const onFilterChange = (event: any) => {
    setFilter({ ...filter, ...event });
  };

  const handleMenuItemClick = (token: Currency) => {
    handleTokenSelect(token);
    handleClose();
  };

  return (
    <Dialog
      onClose={handleClose}
      aria-labelledby="simple-dialog-title"
      open={open}
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
            <Box display="flex" alignItems="center">
              <Box className={cx(classes.tokenIcon)}>
                <img
                  src={require(`assets/coins/${item.unit}.png`).default}
                  alt="token icon"
                />
              </Box>
              <Box className={cx(classes.tokenName)}>
                <Box>{item.unit.toUpperCase()}</Box>
                <Box>{"exDANA"}</Box>
              </Box>
            </Box>
            <Box className={cx(classes.amount)}>{item.quantity.toNumber()}</Box>
          </ListItem>
        ))}
      </List>
    </Dialog>
  );
};

export default TokenSelectorDialog;
