import React from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";
import ArrowLeft from "assets/svg/arrow-left.svg";
import ArrowRight from "assets/svg/arrow-right.svg";

const useStyles = makeStyles(({ palette }) => ({
  dropdown: {
    cursor: "pointer",
    color: palette.common.white,
    flexDirection: "column",
    justifyContent: "center",
    alignItems: "center",
  },
  arrow: {
    width: "14px",
    height: "14px",
    transition: "all .2s ease-in-out",
  },
  arrowMobile: {
    width: "10px",
    height: "10px",
  },
  isOpen: {
    transform: "rotate(180deg)",
    marginTop: "-3px",
  },
}));

const defaultSize = {
  desktop: "46px",
  mobile: "46px",
};

export interface SwapButtonProps {
  isOpen?: boolean;
  style?: object;
}

const SwapButton: React.FC<SwapButtonProps> = ({
  isOpen = false,
  style = {},
}) => {
  const { palette, breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  return (
    <Box
      width={mobile ? defaultSize.mobile : defaultSize.desktop}
      height={mobile ? defaultSize.mobile : defaultSize.desktop}
      borderRadius={"100%"}
      bgcolor={palette.primary.main}
      display={"flex"}
      className={cx(classes.dropdown)}
      style={style}
    >
      <img
        src={ArrowLeft}
        alt={"<"}
        className={cx(classes.arrow)}
      />
      <img
        src={ArrowRight}
        alt={">"}
        className={cx(classes.arrow)}
      />
    </Box>
  );
};

export default SwapButton;
