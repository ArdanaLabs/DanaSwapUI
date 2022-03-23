import React from "react"
import cx from "classnames"
import { BootstrapInput, SearchInput } from "components"
import { makeStyles } from "@mui/styles"
import {
  Box,
  MenuItem,
  Select,
  useMediaQuery,
  useTheme,
  Theme,
  SelectChangeEvent,
} from "@mui/material"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    display: "flex",
    justifyContent: "space-between",
    margin: "20px 0px",
  },
  typographyPrimary: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
  },
  typographySecondary: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 100,
  },
  filterType: {
    display: "flex",
  },
  filterItem: {
    borderWidth: 1,
    borderStyle: "solid",
    borderColor: "transparent",
    borderRadius: "20px",
    fontSize: "16px",
    lineHeight: "100%",
    display: "flex",
    alignItems: "center",
    background: "transparent",
    cursor: "pointer",
    color: theme.palette.primary.main,
    padding: "5px 20px",
    marginRight: "20px",

    [`&.active`]: {
      background: theme.palette.info.light,
      color: theme.palette.common.white,
    },
    [`&:hover`]: {
      borderColor: theme.palette.primary.main,
    },
  },
  searchBox: {
    [theme.breakpoints.down("sm")]: {
      width: "100%",
    },
  },
  menuItem: {
    color: theme.palette.primary.main,
  },
}))

export enum FilterType {
  Popular = "Popular Assets",
  All = "All Assets",
  Your = "Your Vaults",
  Stablecoins = "Stablecoins",
  LiquidityPool = "LP Token",
}

export interface FilterOption {
  filterType: FilterType
  keyword: string
}

export interface TokenAssetGridFilterProps {
  filterOption: FilterOption
  avFilterTypes: FilterType[]
  handleFilterChange: (filterOption: FilterOption) => void
}

const TokenAssetGridFilter: React.FC<TokenAssetGridFilterProps> = ({
  filterOption,
  avFilterTypes,
  handleFilterChange,
}) => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down("sm"))
  const classes = useStyles(theme)
  const { filterType, keyword } = filterOption

  const hanldeFilterTypeChange = (type: FilterType) => {
    handleFilterChange({
      ...filterOption,
      filterType: type,
    })
  }

  const handleFilterInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    handleFilterChange({
      ...filterOption,
      keyword: e.target.value,
    })
  }

  return (
    <Box className={classes.root} flexDirection={mobile ? "column" : "row"}>
      {!mobile ? (
        <Box className={classes.filterType}>
          {avFilterTypes.map((avFilterType) => (
            <Box
              key={avFilterType}
              className={cx(classes.typographyPrimary, classes.filterItem, {
                active: filterType === avFilterType,
              })}
              onClick={() => hanldeFilterTypeChange(avFilterType)}
            >
              {avFilterType}
            </Box>
          ))}
        </Box>
      ) : (
        <Box mb="20px" width="100%">
          <Select
            labelId="Filter"
            id="Filter Select"
            value={filterOption.filterType}
            onChange={(event: SelectChangeEvent<FilterType>) => {
              hanldeFilterTypeChange(event.target.value as FilterType)
            }}
            input={<BootstrapInput />}
          >
            {avFilterTypes.map((avFilterType) => (
              <MenuItem
                value={avFilterType}
                key={avFilterType}
                className={cx(classes.menuItem)}
              >
                {avFilterType}
              </MenuItem>
            ))}
          </Select>
        </Box>
      )}

      <Box>
        <SearchInput
          value={keyword}
          onChange={handleFilterInputChange}
          className={cx(classes.searchBox)}
        />
      </Box>
    </Box>
  )
}

export default TokenAssetGridFilter
