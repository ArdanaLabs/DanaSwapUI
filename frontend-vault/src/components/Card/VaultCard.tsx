import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"
import { VaultButton } from "components"
import { currencyFormatter, percentageFormatter } from "hooks"
import { useUiModal } from "state/ui/hooks"
import { MyVaultInfo } from "state/wallet/reducer"

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
  row: MyVaultInfo
}

const VaultCard: React.FC<VaultCardProps> = ({ row }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const { toggleModal } = useUiModal()

  const handleOpenVault = () => {
    toggleModal({
      open: true,
      type: row.type,
    })
  }

  return (
    <Box className={cx(classes.root)}>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Asset
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          <img src={`assets/image/coins/${row.asset}.svg`} alt="" />
          {row.asset}
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          VaultID
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          #{row.id}
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Liquidation Price
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {currencyFormatter(row.locked)}
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Colt Ratio
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {percentageFormatter(row.collRatio, 0)}
        </Box>
      </Box>
      <Box className={cx(classes.row)}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          DAI Debt
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {row.debt}
        </Box>
      </Box>
      <Box justifyContent="center" display="flex" mt="20px">
        <VaultButton onClick={handleOpenVault}>Manage Vault</VaultButton>
      </Box>
    </Box>
  )
}

export default VaultCard
