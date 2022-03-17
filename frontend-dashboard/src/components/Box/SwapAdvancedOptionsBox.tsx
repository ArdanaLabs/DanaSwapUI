import React, { ChangeEvent, useState, useEffect, useRef } from "react"
import cx from "classnames"
import {
  Box,
  Collapse,
  Typography,
  useMediaQuery,
  RadioGroup,
  Radio,
  FormControlLabel,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useUserTheme } from "state/user/hooks"
import * as Theme from "Data/User/Theme"

import { ReactComponent as ChevUpIcon } from "assets/imgs/chev-up.svg"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    padding: "0 20px",
  },
  header: {
    display: "flex",
    alignItems: "center",
    marginBottom: 20,

    [`& > .title`]: {
      color: palette.primary.main,
      textTransform: "uppercase",
    },

    [`& > svg`]: {
      marginLeft: 10,
      cursor: "pointer",
      transition: "transform .2s",

      [`& path`]: {
        stroke: palette.primary.main,
      },
      [`&.collapsed`]: {
        transform: "rotateZ(180deg)",
      },
    },
  },
  body: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "flex-start",

    [breakpoints.down("xs")]: {
      flexDirection: "column",
    },
  },

  radioGroup: {
    [breakpoints.down("xs")]: {
      flexDirection: "row",
    },
  },

  radioGroupTitle: {
    textTransform: "uppercase",
    color: palette.secondary.main,
  },

  formControlLabel: {
    margin: 0,
    lineHeight: 0,

    [breakpoints.down("xs")]: {
      marginRight: 20,
    },
  },
  optionText: {
    color: palette.primary.main,
    fontSize: 14,
    fontWeight: 400,
  },

  styledRadio: {
    padding: "7px 10px 7px 0 !important",
    [`&:hover`]: {
      backgroundColor: "transparent",
    },
  },
  styledRadioIcon: {
    borderRadius: "50%",
    width: 18,
    height: 18,
    background: "transparent",
    border: `1px solid ${palette.primary.main}`,

    [breakpoints.down("xs")]: {
      width: 14,
      height: 14,
    },
  },
  styledRadioCheckedIcon: {
    background: `${palette.secondary.main}88`,
  },

  slippageAmountInput: {
    width: 100,
    height: 18,
    borderRadius: 50,
    background: `${palette.secondary.main}32`,
    border: "none",
    outline: "none",
    textAlign: "center",
    fontFamily: "Museo Sans",
    fontWeight: 600,
    fontSize: "9px",
    color: palette.primary.main,

    [`&::placeholder`]: {
      color: palette.primary.main,
    },
    [`&::-webkit-outer-spin-button, &::-webkit-inner-spin-button`]: {
      [`-webkit-appearance`]: "none",
      margin: 0,
    },
  },
}))

export enum FilterOptionType {
  Insert = "Insert",
  Filter = "Filter",
  Names = "Names",
  Here = "Here",
  Accordingly = "Accordingly",
}

export enum GasPriceOptionType {
  Slow = 20.5,
  Standard = 25,
  Fast = 28,
  Instant = 32,
}

interface Props {
  filterOption: FilterOptionType
  slippage: number
  gasPriceOption: GasPriceOptionType
  handleFilterOptionChange: (filterOption: FilterOptionType) => void
  handleSlippageChange: (slippage: number) => void
  handleGasPriceOptionChange: (gasPriceOption: GasPriceOptionType) => void
}

const SwapAdvancedOptionsBox: React.FC<Props> = ({
  filterOption,
  slippage,
  gasPriceOption,
  handleFilterOptionChange,
  handleSlippageChange,
  handleGasPriceOptionChange,
}) => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })
  const [collapsed, setCollapsed] = useState(false)

  const [customSlippageAmount, setCustomSlippageAmount] = useState<
    number | undefined
  >(undefined)

  const slippageAmountRef = useRef<HTMLInputElement>(null)

  useEffect(() => {
    if (customSlippageAmount !== undefined) {
      handleSlippageChange(customSlippageAmount)
      setTimeout(
        () => slippageAmountRef.current && slippageAmountRef.current.focus(),
        10
      )
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [customSlippageAmount])

  const StyledRadio = (props: any) => (
    <Radio
      className={classes.styledRadio}
      disableRipple
      color="default"
      checkedIcon={
        <span
          className={cx(
            classes.styledRadioIcon,
            classes.styledRadioCheckedIcon
          )}
        />
      }
      icon={<span className={classes.styledRadioIcon} />}
      {...props}
    />
  )

  const StyledFormControlLabel = (props: any) => (
    <FormControlLabel
      className={classes.formControlLabel}
      value={props.value}
      control={<StyledRadio />}
      label={props.label}
    />
  )

  const SlippageAmountInput = () => (
    <input
      ref={slippageAmountRef}
      className={cx(classes.slippageAmountInput)}
      placeholder={"Custom Amount"}
      type="number"
      value={customSlippageAmount}
      onChange={(e: ChangeEvent<HTMLInputElement>) =>
        setCustomSlippageAmount(Number(e.target.value))
      }
    />
  )

  const renderHeader = () => (
    <Box className={classes.header}>
      <Typography variant="h4" component="span" className="title">
        Advanced Options
      </Typography>
      <ChevUpIcon
        className={cx({ collapsed: collapsed })}
        onClick={() => setCollapsed(!collapsed)}
      />
    </Box>
  )

  const renderFilterOptionGroup = () => (
    <Box>
      <Box mb={1}>
        <Typography
          variant="h4"
          component="h4"
          className={classes.radioGroupTitle}
        >
          Filter
        </Typography>
      </Box>
      <RadioGroup
        className={classes.radioGroup}
        aria-label={"Filter"}
        name={"Filter"}
        value={filterOption}
        onChange={(e: ChangeEvent<HTMLInputElement>): void =>
          handleFilterOptionChange(e.target.value as FilterOptionType)
        }
      >
        {[
          FilterOptionType.Insert,
          FilterOptionType.Filter,
          FilterOptionType.Names,
          FilterOptionType.Here,
          FilterOptionType.Accordingly,
        ].map((optionText) => (
          <StyledFormControlLabel
            key={optionText}
            value={optionText}
            label={
              <Typography
                variant="h4"
                component="span"
                className={classes.optionText}
              >
                {optionText}
              </Typography>
            }
          />
        ))}
      </RadioGroup>
    </Box>
  )

  const renderSlippageGroup = () => (
    <Box>
      <Box mb={1}>
        <Typography
          variant="h4"
          component="span"
          className={classes.radioGroupTitle}
        >
          Slippage
        </Typography>
      </Box>
      <RadioGroup
        className={classes.radioGroup}
        aria-label={"Slippage"}
        name={"Slippage"}
        value={slippage}
        onChange={(e: ChangeEvent<HTMLInputElement>): void => {
          const value = Number(e.target.value) as number
          if (value === 0.5 || value === 1) {
            setCustomSlippageAmount(undefined)
          }
          handleSlippageChange(value)
        }}
      >
        <StyledFormControlLabel
          value={0.5}
          label={
            <Typography
              variant="h4"
              component="span"
              className={classes.optionText}
            >
              0.5%
            </Typography>
          }
        />
        <StyledFormControlLabel
          value={1}
          label={
            <Typography
              variant="h4"
              component="span"
              className={classes.optionText}
            >
              1%
            </Typography>
          }
        />
        <StyledFormControlLabel
          value={customSlippageAmount ?? 0}
          label={<SlippageAmountInput />}
        />
      </RadioGroup>
    </Box>
  )

  const renderGasPriceGroup = () => (
    <Box>
      <Box mb={1}>
        <Typography
          variant="h4"
          component="span"
          className={classes.radioGroupTitle}
        >
          Gas Prices
        </Typography>
      </Box>
      <RadioGroup
        className={classes.radioGroup}
        aria-label={"Gas Price"}
        name={"Gas Price"}
        value={gasPriceOption}
        onChange={(e: ChangeEvent<HTMLInputElement>): void =>
          handleGasPriceOptionChange(
            Number(e.target.value) as GasPriceOptionType
          )
        }
      >
        {[
          [GasPriceOptionType.Slow, "Slow"],
          [GasPriceOptionType.Standard, "Standard"],
          [GasPriceOptionType.Fast, "Fast"],
          [GasPriceOptionType.Instant, "Instant"],
        ].map(([optionText, speed]) => (
          <StyledFormControlLabel
            key={optionText}
            value={optionText}
            label={
              <Typography
                variant="h4"
                component="span"
                className={classes.optionText}
              >
                {optionText} {speed}
              </Typography>
            }
          />
        ))}
      </RadioGroup>
    </Box>
  )

  const renderBody = () => (
    <Box className={classes.body}>
      {renderFilterOptionGroup()}
      {renderSlippageGroup()}
      {renderGasPriceGroup()}
    </Box>
  )

  return (
    <Box className={classes.root}>
      {renderHeader()}
      <Collapse in={!collapsed}>{renderBody()}</Collapse>
    </Box>
  )
}

export default SwapAdvancedOptionsBox
