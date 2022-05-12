import React, { useEffect } from "react"
import { Box } from "@material-ui/core"

import { GuideLinesSection, MainSection, PressKitSection } from "./sections"

const BrandAssets: React.FC = () => {
  useEffect(() => {
    window.scrollTo(0, 0)
  }, [])
  return (
    <Box>
      <MainSection />
      <GuideLinesSection />
      <PressKitSection />
    </Box>
  )
}

export default BrandAssets
