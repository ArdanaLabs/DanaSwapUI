import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"

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
    "& > i": {
      position: "absolute",
      left: "20px",
      top: "13px",
      fontSize: "12px",
      color: palette.primary.main,
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
      <i className="fa fa-search" aria-hidden="true"></i>
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
