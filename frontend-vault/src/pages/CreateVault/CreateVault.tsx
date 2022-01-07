import React from "react"
import { Box } from "@material-ui/core"
import {
  HeroSection,
  StatSection,
  BorrowSection,
  MultiplySection,
} from "./sections"

export const CreateVaultMultiply: React.FC = () => {
  return (
    <Box>
      <HeroSection />
      <StatSection />
      <MultiplySection />
    </Box>
  )
}

export const CreateVaultBorrow: React.FC = () => {
  return (
    <Box>
      <HeroSection />
      <StatSection />
      <BorrowSection />
    </Box>
  )
}
