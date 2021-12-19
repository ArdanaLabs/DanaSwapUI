import React from "react"
import { Box } from "@material-ui/core"
import { HeroSection, VaultStatSection } from "./sections"

const MyVaults: React.FC = () => {
  return (
    <Box>
      <HeroSection />
      <VaultStatSection />
    </Box>
  )
}

export default MyVaults
