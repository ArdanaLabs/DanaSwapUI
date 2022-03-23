import React, { useMemo } from "react"
import cx from "classnames"
import { useParams } from "react-router-dom"
import { useVault } from "state/vault/hooks"
import { currencyFormatter, percentageFormatter } from "hooks"
import { ReactComponent as CircleQuestionIcon } from "assets/image/svgs/circle-question.svg"

import {
  useMediaQuery,
  Box,
  useTheme,
  Theme,
  Typography,
  Container,
} from "@mui/material"
import { makeStyles } from "@mui/styles"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    padding: "30px 0px",
  },

  wrapper: {
    display: "flex",
    alignItems: "center",
    flexWrap: "wrap",
    padding: "0 8px",

    [theme.breakpoints.down("sm")]: {
      flexDirection: "column",
      alignItems: "start",
    },
  },

  individual: {
    display: "flex",
    alignItems: "center",
    marginBottom: "10px",
    gap: "0 5px",
    color: theme.palette.primary.main,

    [`& h5, & h6`]: {
      color: "currentColor",
    },

    [`& path`]: {
      fill: "currentColor",
    },
    [`& rect`]: {
      stroke: "currentColor",
    },
  },

  border: {
    paddingRight: "20px",
    marginRight: "20px",
    borderRight: `1px solid ${theme.palette.info.dark}`,
  },
}))

const StatSection: React.FC = () => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down("sm"))
  const classes = useStyles(theme)

  const { type } = useParams<{ type: string }>()
  const { vaults } = useVault()

  const vaultInfo = useMemo(
    () => vaults.find((vault) => vault.type === type),
    [type, vaults]
  )

  const renderIndividual = (
    label: string,
    value: string,
    border: boolean = true
  ): JSX.Element => (
    <Box
      className={cx(classes.individual, {
        [classes.border]: border && !mobile,
      })}
    >
      <Typography component="h6" variant="h6">
        {label}:
      </Typography>
      <Typography component="h5" variant="h5">
        <strong>{value}</strong>
      </Typography>
      <CircleQuestionIcon />
    </Box>
  )

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box className={cx(classes.wrapper)}>
          {renderIndividual("VaultID", "TBD")}
          {renderIndividual(
            "Stability Fee",
            percentageFormatter(vaultInfo?.stabilityFee ?? 0)
          )}
          {renderIndividual(
            "Liquidation Fee",
            percentageFormatter(vaultInfo?.liquidationFee ?? 0, 0)
          )}
          {renderIndividual(
            "Min. Collateral Ratio",
            percentageFormatter(vaultInfo?.minCollRatio ?? 0, 0)
          )}
          {renderIndividual(
            "Dust Limit",
            currencyFormatter(vaultInfo?.dustLimit ?? 0),
            false
          )}
        </Box>
      </Container>
    </Box>
  )
}

export default StatSection
