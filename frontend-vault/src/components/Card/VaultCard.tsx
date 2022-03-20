import React from "react"
import cx from "classnames"
import { VaultButton } from "components"
import { currencyFormatter, percentageFormatter } from "hooks"
import { useUiModal } from "state/ui/hooks"
import { MyVaultInfo } from "state/wallet/types"
import { Box, Theme, Typography, useTheme } from "@mui/material"
import { makeStyles } from "@mui/styles"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    borderWidth: 1,
    borderStyle: "solid",
    borderColor: `${theme.palette.primary.main}88`,
    borderRadius: "30px",
    padding: "20px 20px",
    color: theme.palette.primary.main,
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
    display: "flex",
    alignItems: "center",
    fontSize: "16px",
    [`& > img`]: {
      width: "20px",
      marginRight: "10px",
    },
  },
  vaultButton: {
    color: theme.palette.common.white,
    lineHeight: "100%",
  },
}))

export interface VaultCardProps {
  row: MyVaultInfo
}

const VaultCard: React.FC<VaultCardProps> = ({ row }) => {
  const theme = useTheme()
  const classes = useStyles(theme)

  const { toggleModal } = useUiModal()

  const handleOpenVault = () => {
    toggleModal({
      open: true,
      type: row.type,
    })
  }

  return (
    <Box className={classes.root}>
      <Box className={classes.row}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Asset
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          <img src={`assets/image/coins/${row.asset}.svg`} alt="" />
          {row.asset}
        </Box>
      </Box>
      <Box className={classes.row}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          VaultID
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          #{row.id}
        </Box>
      </Box>
      <Box className={classes.row}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Liquidation Price
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {currencyFormatter(row.locked)}
        </Box>
      </Box>
      <Box className={classes.row}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Colt Ratio
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {percentageFormatter(row.collRatio, 0)}
        </Box>
      </Box>
      <Box className={classes.row}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          DAI Debt
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {row.debt}
        </Box>
      </Box>
      <Box justifyContent="center" display="flex" mt="20px">
        <VaultButton onClick={handleOpenVault}>
          <Typography variant="h3" className={classes.vaultButton}>
            Manage Vault
          </Typography>
        </VaultButton>
      </Box>
    </Box>
  )
}

export default VaultCard
