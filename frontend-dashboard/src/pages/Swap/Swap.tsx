import React, { useState, ChangeEvent } from "react"
import {
  Box,
  // Collapse,
  Fade,
  // Grid,
  Mark,
  useMediaQuery,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { TokenBox } from "components/Box"
import {
  SelectAssetModal,
  Slider,
  SwapDetailBox,
  SwapButton,
  SwapAdvancedOptionsBox,
} from "components"
// import { options } from "data"
import { Currencies } from "data"

// import CircleInfoCyanIcon from "assets/icons/circle-info-cyan.png"

import CyanBG from "assets/backgrounds/cyan.svg"
import PinkBG from "assets/backgrounds/pink.svg"
import {
  FilterOptionType,
  GasPriceOptionType,
} from "components/Box/SwapAdvancedOptionsBox"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    background: `url(${PinkBG}) right -600px top -600px no-repeat,
                  url(${CyanBG}) left -800px top -200px no-repeat`,
    paddingTop: "180px",
    paddingBottom: "50px",

    display: "flex",
    justifyContent: "center",
    alignItems: "center",
  },

  panel: {
    width: "500px",
    background: `linear-gradient(126.33deg, ${palette.background.paper} 9.83%, #00000000 96.44%);`,
    boxShadow: "inset 0px 4px 4px rgba(0, 0, 0, 0.25)",
    backdropFilter: "blur(4px)",
    borderRadius: "10px",
  },

  panelHeader: {
    position: "relative",
    width: "100%",
    background: `linear-gradient(126.33deg, ${palette.background.paper} 9.83%, #00000000 96.44%);`,
    borderRadius: "5px",
    boxShadow: "0px 4px 4px rgba(0, 0, 0, 0.25)",

    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "15px",
    lineHeight: "100%",
    color: palette.secondary.main,

    padding: "20px",
    textAlign: "center",

    [`& h2`]: {
      textTransform: "uppercase",
      color: palette.primary.main,
    },
  },

  panelBody: {
    padding: "20px 15px",
  },

  leftBorder: {
    position: "absolute",
    left: 0,
    top: 0,
    bottom: 0,
    width: "5px",
    height: "100%",
    background: `linear-gradient(180deg, ${palette.secondary.main} 0%, ${palette.secondary.dark} 100%)`,
    fontFamily: "auto",
  },

  slider: {
    display: "flex",
    alignItems: "center",
    margin: "10px 0px 10px 10px",
  },

  info: {
    display: "flex",
    alignItems: "center",
    justifyContent: "space-between",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "15px",
    lineGeight: "100%",
    color: palette.secondary.main,
  },

  optionTitle: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 500,
    fontSize: "16px",
    lineHeight: "24px",
    color: palette.type === "light" ? palette.text.primary : palette.text.hint,
    display: "flex",
    alignItems: "center",
    cursor: "pointer",
  },

  submit: {
    background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
    borderRadius: "20px",
    width: "250px",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: "bold",
    fontSize: "24px",
    lineHeight: "100%",
    textAlign: "center",
    color: palette.common.white,
    padding: "15px",
    cursor: "pointer",

    [`&:hover`]: {
      background: "linear-gradient(-90deg, #5F72FF 0%, #73D6F1 100%)",
    },
  },
}))

const sliderMarks: Mark[] = [
  {
    value: 0,
    label: "0%",
  },
  {
    value: 100,
    label: "100%",
  },
]
export interface Currency {
  denom: string
  minimalDenom: string
  decimals: number
  imageUrl: string
}

const Swap: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const [fromAmount, setFromAmount] = useState(0)
  const [toAmount, setToAmount] = useState(0)

  const [fromToken, setFromToken] = useState<Currency>(Currencies[0])
  const [toToken, setToToken] = useState<Currency>(Currencies[1])
  const [selectAssetModalOpen, setSelectAssetModalOpen] = useState(0) //  0: close , 1: fromToken , 2: toToken
  const [filterOption, setFilterOption] = useState<FilterOptionType>(
    FilterOptionType.INSERT
  )
  const [slippage, setSlippage] = useState<number>(0.5)
  const [gasPriceOption, setGasPriceOption] = useState<GasPriceOptionType>(
    GasPriceOptionType.TWENTYEIGHT
  )

  const handleSwapTokens = () => {
    setFromToken(toToken)
    setToToken(fromToken)

    setFromAmount(toAmount)
    setToAmount(fromAmount)
  }

  const renderPanelHeader = (): JSX.Element => (
    <Box className={cx(classes.panelHeader)}>
      <Box className={cx(classes.leftBorder)} />
      <Typography component="h2" variant="h2">
        Swap using all Ardana pools
      </Typography>
    </Box>
  )

  const renderPanelBody = (): JSX.Element => (
    <Box className={cx(classes.panelBody)}>
      <TokenBox
        label="Send"
        token={fromToken}
        amount={fromAmount}
        handleAmountChange={(e) => {
          setFromAmount(e)
        }}
        handleOpenSelectAssetModal={() => setSelectAssetModalOpen(1)}
      />
      <Box className={cx(classes.slider)}>
        <Slider
          min={0}
          max={100}
          defaultValue={0}
          value={typeof fromAmount === "number" ? fromAmount : 0}
          onChange={(e: ChangeEvent<{}>, value: number | number[]): void => {
            setFromAmount(value as number)
          }}
          step={1}
          marks={sliderMarks}
        />
        <Box margin={"0 20px"}>
          <SwapButton handleClick={handleSwapTokens} />
        </Box>
      </Box>
      <TokenBox
        label="Receive"
        token={toToken}
        amount={toAmount}
        handleAmountChange={(e) => {
          setToAmount(e)
        }}
        handleOpenSelectAssetModal={() => setSelectAssetModalOpen(2)}
      />

      <Box mt={3}>
        <SwapDetailBox
          fromTokenDenom={fromToken.denom}
          toTokenDenom={toToken.denom}
          fee={1.005}
          slip={0.01}
          tradeRoute={"exDANA"}
        />
      </Box>

      <Box mt={3}>
        <SwapAdvancedOptionsBox
          filterOption={filterOption}
          slippage={slippage}
          gasPriceOption={gasPriceOption}
          handleFilterOptionChange={(value: FilterOptionType) => {
            setFilterOption(value)
          }}
          handleSlippageChange={(value: number) => {
            setSlippage(value)
          }}
          handleGasPriceOptionChange={(value: GasPriceOptionType) => {
            setGasPriceOption(value)
          }}
        />
      </Box>
      {/*
    <Box padding="10px">
      <Box
        className={cx(classes.optionTitle)}
        onClick={onToggleOptions}
      >
        <Box mr={"7px"}>Advanced Options</Box>
        <i
          className={`fa fa-chevron-${!isOptionOpen ? "up" : "down"}`}
          aria-hidden="true"
        ></i>
      </Box>
      <Collapse in={isOptionOpen}>
        <Grid
          container
          spacing={mobile ? 1 : 2}
          style={{ marginTop: "10px" }}
        >
          {options.map((option, i) => (
            <Grid container item xs={4} key={i}>
              <Radio option={option} value={option.data[0].value} />
            </Grid>
          ))}
        </Grid>
      </Collapse>
    </Box>

    <Box mt="50px"></Box>

    <Box display="flex" justifyContent="center">
      <Box className={cx(classes.submit)}>SWAP</Box>
    </Box> */}
    </Box>
  )

  return (
    <Fade in={true}>
      <Box className={cx(classes.root)}>
        <Box className={cx(classes.panel)}>
          {renderPanelHeader()}
          {renderPanelBody()}
        </Box>

        <SelectAssetModal
          open={selectAssetModalOpen}
          handleClose={() => setSelectAssetModalOpen(0)}
          handleTokenChanged={(newToken) => {
            if (selectAssetModalOpen === 1) {
              setFromToken(newToken)
            } else if (selectAssetModalOpen === 2) {
              setToToken(newToken)
            }
            setSelectAssetModalOpen(0)
          }}
        />
      </Box>
    </Fade>
  )
}

export default Swap
