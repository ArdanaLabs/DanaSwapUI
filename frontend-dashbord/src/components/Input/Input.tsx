import React from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette }) => ({
  input: {
    "& input": {
      borderRadius: "5px",
      border: "unset",
      color: "#979797",
      // background: palette.background.default,
      "&:focus-visible": {
        outline: "unset",
      },
    },
    "& ::placeholder": {
      color: "#979797",
    },
    "& ::-webkit-outer-spin-button, & ::-webkit-inner-spin-button": {
      "-webkit-appearance": "none",
      margin: 0,
    },
  },
}));

export interface InputProps {
  value?: string | number;
  placeholder?: string;
  className?: string;
  type?: string;
  step?: number;
  onChange?: (e: any) => void;
  min?: number;
  max?: number;
}

const Input: React.FC<InputProps> = ({
  value = "",
  className,
  placeholder,
  onChange,
  type = "text",
  step = 0.1,
  min = 0,
  max = 100000000,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  return (
    <Box className={cx(classes.input)}>
      <input
        placeholder={placeholder}
        value={value}
        className={className}
        type={type}
        step={step}
        min={min}
        max={max}
        onChange={onChange}
      ></input>
    </Box>
  );
};

export default Input;
