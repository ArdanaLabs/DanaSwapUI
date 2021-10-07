import React, { useState } from "react";
import { Box, useMediaQuery, Select, InputBase } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";

import { DropdownButton } from "components/Button";

const useStyles = makeStyles(({ palette }) => ({
  select: {
    "& div:first-child": {
      paddingRight: "unset",
      "&:focus": {
        backgroundColor: "unset !important",
      },
      cursor: "unset",
    },
    "& div:nth-of-type(2)": {
      marginRight: "5px",
      cursor: "pointer",
    },
  },
}));

export interface CoinSelectProps {
  value?: any;
  className?: any;
  onChange?: any;
  displayEmpty?: any;
  inputProps?: any;
  children?: any;
}

const CoinSelect: React.FC<CoinSelectProps> = ({
  value = "",
  className = "",
  onChange,
  displayEmpty = false,
  inputProps = {},
  children,
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  const [showOptions, setShowOptions] = useState(false);

  const onToggleOptions = () => {
    setShowOptions((prev) => !prev);
  };

  return (
    <Box className={cx(classes.select)}>
      <Select
        value={value}
        onChange={onChange}
        displayEmpty={displayEmpty}
        className={className}
        inputProps={inputProps}
        input={<InputBase />}
        open={showOptions}
        onClose={() => setShowOptions(false)}
        IconComponent={() => (
          <Box onClick={onToggleOptions}>
            <DropdownButton />
          </Box>
        )}
        renderValue={(option) => <>{option !== "" ? option : "Choose Coin"}</>}
      >
        {children}
      </Select>
    </Box>
  );
};

export default CoinSelect;
