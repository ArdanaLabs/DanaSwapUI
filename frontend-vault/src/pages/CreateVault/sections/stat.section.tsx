import React, { useMemo } from "react"
import { Box, Container, Typography, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useParams } from "react-router-dom"

import { useIsDarkMode } from "state/user/hooks"
import { useVault } from "state/vault/hooks"
import { currencyFormatter, percentageFormatter } from "hooks"
import { ReactComponent as CircleQuestionIcon } from "assets/image/icons/circle-question.svg"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    padding: "30px 0px",
  },

  wrapper: {
    display: "flex",
    alignItems: "center",
    flexWrap: "wrap",

    [breakpoints.down("xs")]: {
      flexDirection: "column",
      alignItems: "start",
    },
  },

  individual: {
    display: "flex",
    alignItems: "center",
    marginBottom: "10px",

    [`& h5, & h6`]: {
      color: palette.primary.main,
    },

    [`& path`]: {
      fill: palette.primary.main,
    },
    [`& rect`]: {
      stroke: palette.primary.main,
    },
  },

  border: {
    paddingRight: "20px",
    marginRight: "20px",
    borderRight: `1px solid ${palette.info.dark}`,
  },
}))

const StatSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

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
      &nbsp;&nbsp;
      <Typography component="h5" variant="h5">
        <strong>{value}</strong>
      </Typography>
      &nbsp;
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
