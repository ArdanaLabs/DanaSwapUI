import React from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette }) => ({
  input: {
    position: "relative",
    "& input": {
      border: "unset",
      fontFamily: "'Museo Sans 300'",
      fontStyle: "normal",

      "&:focus-visible": {
        outline: "unset",
      },
    },
    "& ::placeholder": {
      color: palette.secondary.main,
    },
    "& ::-webkit-outer-spin-button, & ::-webkit-inner-spin-button": {
      "-webkit-appearance": "none",
      margin: 0,
    },

    "& > i": {
      position: "absolute",
      right: "25px",
      top: "13px",
      fontSize: "18px",
      color: palette.common.black,
    },
  },
}));

export interface SearchInputProps {
  value?: any;
  placeholder?: any;
  className?: any;
  onChange?: any;
  isIcon?: boolean;
}

const SearchInput: React.FC<SearchInputProps> = ({
  value = "",
  className = "",
  placeholder = "Search Input",
  isIcon = false,
  onChange,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.input)}>
      <input
        type="text"
        className={className}
        placeholder={placeholder}
        value={value}
        onChange={onChange}
      ></input>
      {isIcon && <i className="fa fa-search" aria-hidden="true"></i>}
    </Box>
  );
};

export default SearchInput;
