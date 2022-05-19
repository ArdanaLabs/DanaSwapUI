import React, { useEffect} from "react"
import { Box } from "@material-ui/core"

import { MainSection } from "./sections"

const Tech: React.FC = () => {
  useEffect(() => {
    window.scrollTo(0, 0)
  }, [])
  return (
    <Box>
      <MainSection />
    </Box>
  )
}

export default Tech
