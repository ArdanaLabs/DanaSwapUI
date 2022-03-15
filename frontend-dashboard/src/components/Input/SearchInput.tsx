import React, { ChangeEvent } from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"

import { ReactComponent as SearchIcon } from "assets/imgs/search.svg"

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
  hasIcon?: boolean
  placeholder?: string
  className?: string
}

const SearchInput: React.FC<SearchInputProps> = ({
  value,
  className = "",
  placeholder = "Search Input",
  hasIcon = false,
  onChange,
}) => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

  return (
    <Box className={cx(classes.input)}>
      <input
        type="text"
        className={className}
        placeholder={placeholder}
        value={value}
        onChange={onChange}
      ></input>
      {hasIcon && <SearchIcon className={classes.icon} aria-hidden="true" />}
    </Box>
  )
}

export default SearchInput
