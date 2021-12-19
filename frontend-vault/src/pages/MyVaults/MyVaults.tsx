import React from "react"
import { Box } from "@material-ui/core"
import { HeroSection, VaultListSection, VaultStatSection } from "./sections"

const MyVaults: React.FC = () => {
  return (
    <Box>
      <HeroSection />
      <VaultStatSection />
      <VaultListSection />
    </Box>
  )
}

export default MyVaults
