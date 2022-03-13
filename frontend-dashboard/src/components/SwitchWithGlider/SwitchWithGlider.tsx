import React from "react"
import {
  Box,
  makeStyles,
  Typography,
  useMediaQuery,
  useTheme,
} from "@material-ui/core"
import cx from "classnames"
import { GradientBox } from "components"
import { useIsDarkMode } from "state/user/hooks"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    display: "inline-flex",
    alignItems: "center",
    gap: "0 10px",
  },
  text: {
    textTransform: "uppercase",
    fontWeight: 900,
    whiteSpace: "pre",
    lineHeight: "100%",
    color: palette.primary.main,

    [`&.isActive`]: {
      color: palette.common.white,
    },
  },
}))

export interface SwitchWithGliderProps {
  elements: string[]
  activeIndex: number
  customClassName?: string
  handleSwitch: (index: number) => void
}

const SwitchWithGlider: React.FC<SwitchWithGliderProps> = ({
  elements,
  activeIndex,
  customClassName = "",
  handleSwitch,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      {elements &&
        elements.map((element: string, index: number) => (
          <GradientBox
            key={element}
            glow={false}
            fill={activeIndex === index}
            onClick={() => handleSwitch(index)}
          >
            <Typography
              variant="h6"
              component="span"
              className={cx(classes.text, customClassName, {
                isActive: activeIndex === index,
              })}
            >
              {element}
            </Typography>
          </GradientBox>
        ))}
    </Box>
  )
}

export default SwitchWithGlider
