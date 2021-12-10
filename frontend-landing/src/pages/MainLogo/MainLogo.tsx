import React from "react"
import { Box } from "@material-ui/core"

import MainLogoImage from "assets/mainlogo.png"

const MainLogo: React.FC = () => {
  return (
    <Box
      display="flex"
      justifyContent="center"
      alignItems="center"
      width="100vw"
      height="100vh"
      style={{ backgroundColor: "#FFF" }}
    >
      <img src={MainLogoImage} alt="main logo" />
    </Box>
  )
}

export default MainLogo
