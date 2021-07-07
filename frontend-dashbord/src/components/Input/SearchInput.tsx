import React from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette }) => ({
  input: {
    "& input": {
      borderRadius: "20px",
      border: "unset",
      color: palette.secondary.main,
      fontFamily: "Museo Sans",
      fontStyle: "normal",
      fontWeight: 600,

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
  },
}));

export interface SearchInputProps {
  value?: any;
  placeholder?: any;
  className?: any;
  onChange?: any;
}

const SearchInput: React.FC<SearchInputProps> = ({
  value = "",
  className = "",
  placeholder = "Search Input",
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
    </Box>
  );
};

export default SearchInput;
