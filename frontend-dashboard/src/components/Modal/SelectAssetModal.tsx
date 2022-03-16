import React, { ChangeEvent, useState } from "react"
import { Dialog, DialogTitle } from "components"
import {
  Box,
  List,
  ListItem,
  useMediaQuery,
  // Typography,
  Button,
  useTheme,
  makeStyles,
  Typography,
} from "@material-ui/core"
import * as Theme from "Data/User/Theme"
import { useUserTheme } from "state/user/hooks"
import { GradientBox, SearchInput } from "components"
import { Currencies, FontFamilies } from "data"
import { Currency } from "pages/Swap/Swap"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  filterByTypeWrapper: {
    display: "flex",
    alignItems: "center",
    gap: "10px",
    textTransform: "uppercase",

    [`& h6`]: {
      color: palette.primary.main,
    },
  },

  filterByTypeButton: {
    background: `linear-gradient(90deg, ${palette.secondary.main} 0%, ${palette.secondary.dark} 100%)`,
    borderRadius: "50px",

    [`& h6`]: {
      color: palette.common.white,
    },
  },

  filterByTextWrapper: {},

  filterByTextInput: {
    background: `${palette.secondary.main}32`,
    borderRadius: "50px",
    fontFamily: FontFamilies.Museo,
    fontSize: "16px",
    fontWeight: 600,
    lineHeight: "100%",
    width: "100%",
    padding: "10px 20px",
    color: palette.primary.main,

    [`&::placeholder`]: {
      color: palette.primary.main,
    },
  },

  menuItem: {
    borderBottom: `1px solid ${palette.primary.main}4D`,
    justifyContent: "space-between",
  },

  tokenImage: {
    marginRight: "10px",
    width: "30px",
    height: "30px",
  },

  tokenDenom: {
    color: palette.primary.main,
    [`& h6`]: {
      fontWeight: "normal",
    },
  },
  balance: {
    color: palette.primary.main,
  },
}))

interface Props {
  open: number
  handleClose: () => void
  handleTokenChanged: (newToken: Currency) => void
}

enum AssetFilterType {
  ALL = "All",
  NATIVE = "Native",
  ERC20 = "ERC20",
  BEP20 = "BEP20",
}

const SelectAssetModal: React.FunctionComponent<Props> = ({
  open,
  handleClose,
  handleTokenChanged,
}) => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })
  const [filter, setFilter] = useState({
    text: "",
    type: AssetFilterType.ALL,
  })

  return (
    <Dialog
      onClose={handleClose}
      aria-labelledby="simple-dialog-title"
      open={open > 0}
    >
      <DialogTitle id="customized-dialog-title" onClose={handleClose}>
        <Typography variant="h2" component="h2">
          Select Asset
        </Typography>
      </DialogTitle>

      <Box className={classes.filterByTypeWrapper} mt={3}>
        {[
          AssetFilterType.ALL,
          AssetFilterType.NATIVE,
          AssetFilterType.ERC20,
          AssetFilterType.BEP20,
        ].map((filterType) =>
          filterType === filter.type ? (
            <Button
              key={filterType}
              variant="contained"
              onClick={() => {
                setFilter({
                  ...filter,
                  type: filterType,
                })
              }}
              className={classes.filterByTypeButton}
            >
              <Typography variant="h6" component="h6">
                {filterType}
              </Typography>
            </Button>
          ) : (
            <GradientBox
              key={filterType}
              width={70}
              height={31}
              glow={false}
              onClick={() => {
                setFilter({
                  ...filter,
                  type: filterType,
                })
              }}
            >
              <Typography variant="h6" component="h6">
                {filterType}
              </Typography>
            </GradientBox>
          )
        )}
      </Box>

      <Box className={classes.filterByTextWrapper} mt={3}>
        <SearchInput
          className={classes.filterByTextInput}
          value={filter.text}
          placeholder="Search..."
          isIcon={true}
          onChange={(e: ChangeEvent<HTMLInputElement>) => {
            setFilter({
              ...filter,
              text: e.target.value,
            })
          }}
        />
      </Box>

      <List>
        {Currencies.map((item: Currency, index) => (
          <ListItem
            button
            className={classes.menuItem}
            onClick={() => handleTokenChanged(item)}
            key={index}
          >
            <Box display="flex" alignItems="center">
              <img
                className={classes.tokenImage}
                src={item.imageUrl}
                alt="token"
              />
              <Box className={classes.tokenDenom}>
                <Typography component="h4" variant="h4">
                  {item.denom}
                </Typography>
                <Typography component="h6" variant="h6">
                  {item.minimalDenom}
                </Typography>
              </Box>
            </Box>
            <Typography
              variant="h2"
              component="span"
              className={classes.balance}
            >
              0
            </Typography>
          </ListItem>
        ))}
      </List>
    </Dialog>
  )
}

export default SelectAssetModal
