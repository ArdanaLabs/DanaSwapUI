import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"
import { VaultButton } from "components"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    border: `1px solid ${palette.primary.main}88`,
    borderRadius: "30px",
    padding: "20px 20px",
    color: palette.primary.main,
    marginBottom: "20px",
  },
  typographyPrimary: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
  },
  typographySecondary: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: "normal",
    opacity: "0.8",
  },
  row: {
    display: "flex",
    justifyContent: "space-between",
    padding: "5px 0px",
  },
  property: {
    fontSize: "16px",
  },
  value: {
    "display": "flex",
    "alignItems": "center",
    "fontSize": "16px",
    "& > img": {
      width: "20px",
      marginRight: "10px",
    },
  },
}))

export interface TokenAssetCardProps {
  id: number
  asset: string
  type: string
  dUSD: string
  stabilityFee: number
  minColl: number
  assetIcon: string
}

const TokenAssetCard: React.FC<TokenAssetCardProps> = ({
  id,
  asset,
  type,
  dUSD,
  stabilityFee,
  minColl,
  assetIcon,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Asset
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          <img src={assetIcon} alt="" />
          {asset}
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Type
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {type}
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          dUSD Available
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {dUSD}
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Stability Fee
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {stabilityFee.toFixed(2)}%
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Min Coll. Ratio
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {minColl}%
        </Box>
      </Box>
      <Box justifyContent="center" display="flex" mt="20px">
        <VaultButton>Open Vault</VaultButton>
      </Box>
    </Box>
  )
}

export default TokenAssetCard
