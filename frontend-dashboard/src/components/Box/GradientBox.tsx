import React, { useEffect, useMemo, useRef, useState } from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useUserTheme } from "state/user/hooks"
import { v4 as uuidv4 } from "uuid"
import * as Theme from "Data/User/Theme"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    cursor: "pointer",
    position: "relative",
    lineHeight: "0px",
    display: "inline-block",
    zIndex: 102,
  },
  children: {
    position: "absolute",
    top: "0px",
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
    width: "100%",
    height: "100%",
  },
  childrenTmp: {
    position: "absolute",
    top: "0px",
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
    visibility: "hidden",
  },
}))

interface GradientBoxProps {
  width?: number
  height?: number
  padding?: number
  strokeWidth?: number
  clickable?: boolean
  glow?: boolean
  fill?: boolean
  onClick: (_event: any) => void
}

const GradientBox: React.FC<GradientBoxProps> = ({
  width = 0,
  height = 0,
  padding = 10,
  strokeWidth = 3,
  clickable = true,
  glow = true,
  fill = false,
  children,
  onClick,
}) => {
  const { breakpoints, palette } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })
  const [hover, setHover] = useState(false)
  const domID = useMemo(() => uuidv4().split("-").pop(), [])

  const timer: { current: NodeJS.Timer | undefined } = useRef(undefined)
  const [childrenDOM, setChildrenDOM] = useState<Element | undefined>(undefined)

  useEffect(() => {
    timer.current = setInterval(() => {
      const dom = document.querySelector(`#grad${domID}`)
      if (dom) {
        setChildrenDOM(dom)
        clearInterval(timer.current as NodeJS.Timer)
      }
    }, 10)
    return () => {
      clearInterval(timer.current as NodeJS.Timer)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  const boxWidth = useMemo(() => {
    if (width === 0) {
      if (childrenDOM) {
        const { width: childWidth } = childrenDOM.getBoundingClientRect()
        return childWidth + padding * 4
      }
    }
    return width
  }, [width, childrenDOM, padding])
  const boxHeight = useMemo(() => {
    if (height === 0) {
      if (childrenDOM) {
        const { height: childHeight } = childrenDOM.getBoundingClientRect()
        return childHeight + padding * 2
      }
    }
    return height
  }, [height, childrenDOM, padding])

  return (
    <Box position="relative" lineHeight={0} display="inline-block">
      <Box
        className={cx(classes.root)}
        onMouseEnter={() => setHover(true)}
        onMouseLeave={() => setHover(false)}
        onClick={onClick}
      >
        <svg width={boxWidth + strokeWidth} height={boxHeight + strokeWidth}>
          <defs>
            <linearGradient id="grad1" x1="0" y1="0" x2="1" y2="0">
              <stop offset="0" stopColor={palette.secondary.dark} />
              <stop offset="1" stopColor={palette.secondary.main} />
            </linearGradient>
          </defs>
          <g fill="none">
            <rect
              x={strokeWidth / 2}
              y={strokeWidth / 2}
              width={boxWidth}
              height={boxHeight}
              rx={boxHeight / 2}
              stroke="url(#grad1)"
              strokeWidth={strokeWidth}
              fill={fill ? "url(#grad1)" : "unset"}
            />
          </g>
        </svg>
        {/* {boxWidth > 0 && boxHeight > 0 && ( */}
        <Box className={cx(classes.children)}>{children}</Box>
        {/* )} */}
        <Box id={`grad${domID}`} className={cx(classes.childrenTmp)}>
          {children}
        </Box>
      </Box>
      <Box
        position="absolute"
        top={strokeWidth + "px"}
        left={strokeWidth + "px"}
        width={`calc(100% - ${strokeWidth * 2}px)`}
        height={`calc(100% - ${strokeWidth * 2}px)`}
        borderRadius="500px"
        border={`${strokeWidth} solid transparent`}
        boxShadow={glow ? "0px 0px 14px 7px #2D3BA0" : "none"}
        zIndex={101}
        style={clickable && hover ? { background: "#FFFFFF33" } : {}}
      />
    </Box>
  )
}

export default GradientBox
