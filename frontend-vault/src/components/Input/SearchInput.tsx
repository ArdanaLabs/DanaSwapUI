import React, { ChangeEvent } from "react"

import { useTheme } from "@mui/system"
import { makeStyles } from "@mui/styles"
import { Theme, Box } from "@mui/material"

import { ReactComponent as SearchIcon } from "assets/image/svgs/search.svg"

const useStyles = makeStyles((theme: Theme) => ({
  input: {
    position: "relative",
    [`& > input`]: {
      background: "transparent",
      borderWidth: 1,
      borderStyle: "solid",
      boderColor: theme.palette.primary.main,
      borderRadius: "50px",
      padding: "10px 10px 10px 40px",
      color: theme.palette.primary.main,

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
        fill: theme.palette.primary.main,
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
