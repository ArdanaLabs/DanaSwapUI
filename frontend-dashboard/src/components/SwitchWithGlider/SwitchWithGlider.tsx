import React from "react"
import { Box, makeStyles } from "@material-ui/core"
import { Button } from "components/Button"
import cx from "classnames"

const useStyles = makeStyles(() => ({
  root: {
    "display": "inline-flex",
    "alignItems": "center",
    "margin": "0px -5px",

    "& > button": {
      margin: "0px 5px",
    },
  },
}))

export interface SwitchWithGliderProps {
  elements: any[]
  vertical?: boolean
  normalClass: any
  activeClass: any
  activeIndex: number
  handleSwitch: any
}

const SwitchWithGlider: React.FC<SwitchWithGliderProps> = ({
  elements,
  vertical,
  normalClass,
  activeClass,
  activeIndex,
  handleSwitch,
}) => {
  const classes = useStyles()
  return (
    <Box className={cx(classes.root)}>
      {elements &&
        elements.map((element: any, i: number) => (
          <Button
            key={i}
            variant="contained"
            onClick={() => handleSwitch(i)}
            className={cx(normalClass, {
              [activeClass]: activeIndex === i,
            })}
          >
            {element}
          </Button>
        ))}
    </Box>
  )
}

export default SwitchWithGlider
