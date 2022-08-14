import React, { ChangeEvent } from "react"

import { useTheme } from "@mui/system"
import { makeStyles } from "@mui/styles"
import { Theme, Box } from "@mui/material"

import { ReactComponent as SearchIcon } from "assets/image/svgs/search.svg"

const useStyles = makeStyles((theme: Theme) => ({
  input: {
    position: "relative",
    color: theme.palette.primary.main,

    [`& > input`]: {
      background: "transparent",
      borderWidth: 1,
      borderStyle: "solid",
      boderColor: "currentColor",
      borderRadius: "50px",
      padding: "10px 10px 10px 40px",
      color: "currentColor",

      [`&:focus-visible`]: {
        outline: "unset",
      },
    },
    [`& > svg`]: {
      position: "absolute",
      left: "25px",
      top: "50%",
      transform: "translate(-50%, -50%)",
      fontSize: "12px",

      [`& path`]: {
        fill: "currentColor",
      },
    },
  },
}))

export interface SearchInputProps {
  value?: string
  placeholder?: string
  className?: string
  onChange?: (value: ChangeEvent<HTMLInputElement>) => void
}

const SearchInput: React.FC<SearchInputProps> = ({
  value = "",
  className = "",
  placeholder = "",
  onChange,
}) => {
  const theme = useTheme()
  const classes = useStyles(theme)

  return (
    <Box className={classes.input}>
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
