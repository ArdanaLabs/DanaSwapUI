import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"

import { ReactComponent as SearchIcon } from "assets/image/icons/search.svg"

const useStyles = makeStyles(({ palette }) => ({
  input: {
    "position": "relative",
    "& > input": {
      "background": "transparent",
      "border": `1px solid ${palette.primary.main}`,
      "borderRadius": "50px",
      "padding": "10px 10px 10px 40px",
      "color": palette.primary.main,

      "&:focus-visible": {
        outline: "unset",
      },
    },
    "& > svg": {
      "position": "absolute",
      "left": "25px",
      "top": "50%",
      "transform": "translate(-50%, -50%)",
      "fontSize": "12px",

      "& path": {
        fill: palette.primary.main,
      },
    },
  },
}))

export interface SearchInputProps {
  value?: any
  placeholder?: any
  className?: any
  onChange?: any
}

const SearchInput: React.FC<SearchInputProps> = ({
  value = "",
  className = "",
  placeholder = "",
  onChange,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.input)}>
      <SearchIcon />
      <input
        type="text"
        className={className}
        placeholder={placeholder}
        value={value}
        onChange={onChange}
      />
    </Box>
  )
}

export default SearchInput
