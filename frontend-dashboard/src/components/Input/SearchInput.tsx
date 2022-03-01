import React, { ChangeEvent } from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"

import { ReactComponent as SearchIcon } from "assets/icons/search.svg"

const useStyles = makeStyles(({ palette }) => ({
  input: {
    position: "relative",
    [`& input`]: {
      border: "unset",
      fontFamily: "Museo Sans",
      fontStyle: "normal",

      [`&:focus-visible`]: {
        outline: "unset",
      },
    },
    [`& ::placeholder`]: {
      color: palette.secondary.main,
    },
    [`& ::-webkit-outer-spin-button, & ::-webkit-inner-spin-button`]: {
      [`-webkit-appearance`]: "none",
      margin: 0,
    },
  },

  icon: {
    position: "absolute",
    top: "50%",
    right: "10px",
    transform: "translate(-50%, -50%)",

    [`& path`]: {
      fill: palette.primary.main,
    },
  },
}))

export interface SearchInputProps {
  value: string | number
  onChange: (e: ChangeEvent<HTMLInputElement>) => void
  isIcon?: boolean
  placeholder?: string
  className?: string
}

const SearchInput: React.FC<SearchInputProps> = ({
  value,
  className = "",
  placeholder = "Search Input",
  isIcon = false,
  onChange,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.input)}>
      <input
        type="text"
        className={className}
        placeholder={placeholder}
        value={value}
        onChange={onChange}
      ></input>
      {isIcon && <SearchIcon className={classes.icon} />}
    </Box>
  )
}

export default SearchInput
