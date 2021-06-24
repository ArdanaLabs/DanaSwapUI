import React from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { useTheme } from "@material-ui/core/styles";

export interface ProfileBoxProps {
  image?: any;
  name?: string;
  job?: string;
  info?: string;
  custom_style?: object;
}

const ProfileBox: React.FC<ProfileBoxProps> = ({
  image,
  name,
  job,
  info,
  custom_style,
}) => {
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));

  return (
    <Box position="relative" flex="2" marginY="20px" style={custom_style}>
      <Box borderRadius="30px">
        <img src={image} alt={name} width="100%" />
      </Box>

      <Box
        color="#2F3DA0"
        fontWeight="900"
        fontSize={!mobile ? "32px" : "24px"}
        pt="10px"
      >
        {name}
      </Box>

      <Box
        color="#423F3F"
        fontSize={!mobile ? "18px" : "16px"}
        fontWeight="300"
        lineHeight="21px"
      >
        {job}
      </Box>

      <Box
        color="#000000"
        fontSize={!mobile ? "18px" : "16px"}
        fontWeight="300"
        lineHeight={!mobile ? "21px" : "16px"}
        whiteSpace="pre-wrap"
        py="15px"
      >
        {info}
      </Box>
    </Box>
  );
};

export default ProfileBox;
