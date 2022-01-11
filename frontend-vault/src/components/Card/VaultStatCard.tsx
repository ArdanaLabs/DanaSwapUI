import React, { useMemo } from "react"
import cx from "classnames"
import {
  Box,
  Container,
  Grid,
  makeStyles,
  Typography,
  useMediaQuery,
  useTheme,
} from "@material-ui/core"
import { useIsDarkMode } from "state/user/hooks"
import { percentageFormatter } from "hooks"
import { MyVaultInfo } from "state/wallet/reducer"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    "background":
      palette.type === "dark"
        ? palette.background.paper
        : "linear-gradient(359deg, #B9D4FF -129.98%, #FFFFFF 99.14%)",
    "borderRadius": "20px",
    "padding": "30px",

    "& h5": {
      textTransform: "uppercase",
    },
    "& h1, & h5, & h6": {
      color: palette.primary.main,
    },

    [breakpoints.down("xs")]: {
      textAlign: "left",
      padding: "30px 10px",
    },
  },

  divider: {
    width: "100%",
    height: "5px",
    background: palette.info.light,
    borderRadius: "55px",
  },

  vault: {
    "& .coin": {
      width: "35px",
      height: "35px",
      background: "white",
      borderRadius: "100%",
    },
  },
}))

interface Props {
  vaultList: MyVaultInfo[]
}

const VaultStatCard: React.FC<Props> = ({ vaultList }) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const totalLockedUSD = useMemo(
    () => vaultList.reduce((prev, current) => prev + current.locked, 0),
    [vaultList]
  )

  const totalDebt = useMemo(
    () => vaultList.reduce((prev, current) => prev + current.debt, 0),
    [vaultList]
  )

  const renderVault = (name: string, image: string, percentage: number) => (
    <Box
      key={name}
      className={cx(classes.vault)}
      display="flex"
      alignItems={"center"}
      mr="20px"
    >
      <Box
        className="coin"
        mr="10px"
        display={"flex"}
        alignItems={"center"}
        justifyContent={"center"}
      >
        <img src={image} alt="name" />
      </Box>
      <Box display="flex" flexDirection={"column"}>
        <Typography variant="h5" component="h5" className="name">
          {name}
        </Typography>
        <Typography variant="h6" component="h6" className="rate">
          {percentageFormatter(percentage)}
        </Typography>
      </Box>
    </Box>
  )

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container>
          <Grid item xs={12} sm={6} md={3}>
            <Typography variant="h5" component="h5">
              No. of vaults
            </Typography>
            <Box mb={"20px"} />
            <Typography variant="h1" component="h1">
              {vaultList.length}
            </Typography>
            <Box mb={"20px"} />
          </Grid>

          <Grid item xs={12} sm={6} md={3}>
            <Typography variant="h5" component="h5">
              Total Locked
            </Typography>
            <Box mb={"20px"} />
            <Typography variant="h1" component="h1">
              {`$${totalLockedUSD.toLocaleString()}`}
            </Typography>
            <Box mb={"20px"} />
          </Grid>

          <Grid item xs={12} sm={6} md={3}>
            <Typography variant="h5" component="h5">
              Total Debt
            </Typography>
            <Box mb={"20px"} />
            <Box display={"flex"} alignItems={"baseline"}>
              <Box>
                <Typography variant="h1" component="h1">
                  {`${totalDebt.toLocaleString()}`}
                </Typography>
              </Box>
              <Box ml={"10px"}>
                <Typography variant="h5" component="h5">
                  {` DAI`}
                </Typography>
              </Box>
            </Box>
            <Box mb={"20px"} />
          </Grid>

          <Grid item xs={12} sm={6} md={3}>
            <Typography variant="h5" component="h5">
              Vaults at risk
            </Typography>
            <Box mb={"20px"} />
            <Typography variant="h1" component="h1">
              {vaultList.filter((vault) => vault.risk).length}
            </Typography>
            <Box mb={"20px"} />
          </Grid>
        </Grid>

        <Box className={cx(classes.divider)} />

        <Box mb={"20px"} />

        <Box display={"flex"}>
          {vaultList.map((vault) => {
            return renderVault(
              vault.asset,
              require(`assets/image/coins/${vault.asset}.svg`).default,
              vault.locked / totalLockedUSD
            )
          })}
        </Box>
      </Container>
    </Box>
  )
}

export default VaultStatCard
