import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"
import { VaultButton } from "components"
import { currencyFormatter, percentageFormatter } from "hooks"
import { useUiModal } from "state/ui/hooks"

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
    fontWeight: 100,
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

export interface VaultCardProps {
  id: number
  vaultId: number
  asset: string
  assetIcon: string
  liquidationPrice: number
  coltRatio: number
  daiDebt: number
}

const VaultCard: React.FC<VaultCardProps> = ({
  id,
  vaultId,
  asset,
  assetIcon,
  liquidationPrice,
  coltRatio,
  daiDebt,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const { toggleModal } = useUiModal()

  const handleOpenVault = () => {
    toggleModal({
      open: true,
      asset,
    })
  }

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
          VaultID
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {vaultId}
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Liquidation Price
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {currencyFormatter(liquidationPrice)}
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Colt Ratio
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {percentageFormatter(coltRatio)}
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          DAI Debt
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {daiDebt}
        </Box>
      </Box>
      <Box justifyContent="center" display="flex" mt="20px">
        <VaultButton onClick={handleOpenVault}>Manage Vault</VaultButton>
      </Box>
    </Box>
  )
}

export default VaultCard
