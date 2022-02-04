import React, { useEffect, useState } from "react"
import { Box, Fade, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useHistory } from "react-router-dom"

import * as O from "fp-ts/Option"
import * as ROM from "fp-ts/ReadonlyMap"
import { RemoteData } from "fp-ts-remote-data"

import { FetchDecodeError } from "Data/FetchDecode"
import { PoolStats } from "Data/Stats/PoolStats"
import * as PoolSetName from "Data/Pool/PoolSetName"

import { StatsSection, ChartSection, TransactionsSection } from "./sections"
import { useIsDarkMode } from "state/user/hooks"
import Button from "components/Button/Button"
import { usePoolStats } from "state/home/hooks"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  filter: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    margin: "20px 0",

    [breakpoints.down("xs")]: {
      "flexDirection": "column",
      "& > div": {
        width: "100%",
      },
    },
  },

  filterType: {
    background: palette.secondary.dark,
    padding: "10px 30px",
    fontSize: "11px",
    lineHeight: "100%",
    marginLeft: "10px",

    [breakpoints.down("xs")]: {
      width: "auto",
    },
  },

  active: {
    background: palette.primary.light,
  },
}))

export type Props = {
  poolSet: PoolSetName.Type
}

export const SpecificPool: React.FC<Props> = ({
  poolSet,
}: Props): JSX.Element => {
  const { breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const dark = useIsDarkMode()
  const classes = useStyles({ dark, mobile })
  const history = useHistory()
  const poolStats: RemoteData<
    FetchDecodeError,
    ReadonlyMap<PoolSetName.Type, PoolStats>
  > = usePoolStats()
  const [specificPoolStats, setSpecificPoolStats] = useState<
    O.Option<PoolStats>
  >(O.none)

  useEffect(() => {
    switch (poolStats._tag) {
      case "Success":
        if (O.isNone(specificPoolStats)) {
          const ps: O.Option<PoolStats> = ROM.lookup(PoolSetName.Eq)(poolSet)(
            poolStats.success
          )
          O.isNone(ps) && history.goBack()
          setSpecificPoolStats(ps)
        }
        break
      default:
        break
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [poolStats])

  function renderPoolStats(ps: PoolStats): JSX.Element {
    // TODO: text-transform: upppercase
    const poolSetStr: string = PoolSetName.iso.unwrap(poolSet)
    return (
      <Fade in={true} key={poolSetStr}>
        <Box>
          <Box textAlign="right">
            <Button
              variant="contained"
              onClick={() => history.push("/swap")}
              className={cx(classes.filterType, classes.active)}
            >
              SWAP
            </Button>
            <Button
              variant="contained"
              onClick={() => history.push("/deposit")}
              className={cx(classes.filterType)}
            >
              DEPOSIT
            </Button>
            <Button
              variant="contained"
              onClick={() => history.push("/withdraw")}
              className={cx(classes.filterType)}
            >
              WITHDRAW
            </Button>
          </Box>
          {/* TODO: elements should *NOT* be used for spacer, this is what styles are for */}
          <Box mt={"30px"} />
          <StatsSection poolStats={ps} poolSet={poolSet} />
          <Box mt={"30px"} />
          <ChartSection poolStats={ps} poolSet={poolSet} />
          <Box mt={"50px"} />
          <TransactionsSection poolSet={poolSet} />
        </Box>
      </Fade>
    )
  }

  switch (poolStats._tag) {
    case "Success":
      return O.fold(
        // this should be covered by the goBack()
        () => (
          <div>
            Error: pool set “{PoolSetName.iso.unwrap(poolSet)}” not found
          </div>
        ),
        renderPoolStats
      )(specificPoolStats)
    case "Pending":
      // TODO:
      return <div>loading …</div>
    case "Failure":
      // Given the `goBack`, this state is impossible
      return (
        <details>
          <summary>TODO error</summary>
          <pre>{JSON.stringify(poolStats.failure, null, 2)}</pre>
        </details>
      )
  }
}

export default SpecificPool
