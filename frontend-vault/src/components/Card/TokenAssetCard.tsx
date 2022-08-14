import React from "react"
import cx from "classnames"
import { Box, useTheme, Theme, Typography } from "@mui/material"
import { makeStyles } from "@mui/styles"

import { VaultButton } from "components"
import { useUiModal } from "state/ui/hooks"
import { VaultInfo } from "state/vault/types"
import { numberFormatter, percentageFormatter } from "hooks"
import { FontFamilies } from "theme"

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
    fontFamily: FontFamilies.Brandon,
    fontStyle: "normal",
    fontWeight: 900,
  },
  typographySecondary: {
    fontFamily: FontFamilies.Museo,
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
    textTransform: "uppercase",
    color: theme.palette.common.white,
    lineHeight: "100%",
  },
}))

export interface TokenAssetCardProps {
  row: VaultInfo
}

const TokenAssetCard: React.FC<TokenAssetCardProps> = ({ row }) => {
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
          Type
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {row.type}
        </Box>
      </Box>
      <Box className={classes.row}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          dUSD Available
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {numberFormatter(row.locked)}
        </Box>
      </Box>
      <Box className={classes.row}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Stability Fee
        </Box>
        <Box className={cx(classes.value, classes.typographySecondary)}>
          {percentageFormatter(row.stabilityFee)}
        </Box>
      </Box>
      <Box className={classes.row}>
        <Box className={cx(classes.property, classes.typographyPrimary)}>
          Min Coll. Ratio
        </Box>
        <Box className={(classes.value, classes.typographySecondary)}>
          {percentageFormatter(row.minCollRatio, 0)}
        </Box>
      </Box>
      <Box justifyContent="center" display="flex" mt="20px">
        <VaultButton onClick={handleOpenVault}>
          <Typography variant="h3" className={classes.vaultButton}>
            Open Vault
          </Typography>
        </VaultButton>
      </Box>
    </Box>
  )
}

export default TokenAssetCard
