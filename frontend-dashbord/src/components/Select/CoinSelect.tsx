import React from "react";
import { Box, useMediaQuery, Select, InputBase } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";

import { DropdownButton } from "components/Button";


const useStyles = makeStyles(({ palette }) => ({
  select: {
    '& div:first-child': {
      paddingRight: 'unset',
      "&:focus": {
        backgroundColor: "unset !important",
      }
    },
    "& div:nth-of-type(2)": {
      marginRight: '5px',
      'pointer-events': 'none',
    }
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
  const { palette, breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  return (
    <Box className={cx(classes.select)}>
      <Select
        value={value}
        onChange={onChange}
        displayEmpty={displayEmpty}
        className={className}
        inputProps={inputProps}
        input={<InputBase/> }
        IconComponent={() => (
          <DropdownButton />
        )}
      >
        {children}
      </Select>
    </Box>
  );
};

export default CoinSelect;
