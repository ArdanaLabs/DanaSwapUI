import React, { useEffect, useState } from "react"
import {
  Box,
  Fade,
  useMediaQuery,
  Container,
  CircularProgress,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useHistory } from "react-router-dom"

import * as O from "fp-ts/Option"
import * as ROM from "fp-ts/ReadonlyMap"
import { RemoteData } from "fp-ts-remote-data"

import { FetchDecodeError } from "Data/FetchDecode"
import { PoolStats } from "Data/Stats/PoolStats"
import * as PoolSetName from "Data/Pool/PoolSetName"

import { StatsSection, ChartSection, TransactionsSection } from "./sections"
import { useIsDarkMode } from "state/user/hooks"
import { usePoolStats } from "state/home/hooks"

import CyanBG from "assets/backgrounds/cyan.svg"
import PinkBG from "assets/backgrounds/pink.svg"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: `url(${PinkBG}) right -600px top -600px no-repeat,
                  url(${CyanBG}) left -800px top -500px no-repeat`,
    paddingTop: "180px",
    paddingBottom: "50px",
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
    const poolSetStr: string = PoolSetName.iso.unwrap(poolSet)
    return (
      <Fade in={true} key={poolSetStr}>
        <Box className={classes.root}>
          <Container>
            {/* TODO: elements should *NOT* be used for spacer, this is what styles are for */}
            <StatsSection poolStats={ps} poolSet={poolSet} />
            <ChartSection poolStats={ps} poolSet={poolSet} />
            {/* <TransactionsSection poolSet={poolSet} /> */}
          </Container>
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
      return (
        <Box
          padding={"100px"}
          display="flex"
          justifyContent="center"
          alignItems="center"
        >
          <CircularProgress />
        </Box>
      )
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
