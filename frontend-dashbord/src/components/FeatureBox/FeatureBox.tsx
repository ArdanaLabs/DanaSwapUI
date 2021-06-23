import React from "react";
import { Box } from "@material-ui/core";

export interface FeatureBoxProps {
  image?: any;
  title?: string;
  content?: string;
  custom_style?: object;
}

const FeatureBox: React.FC<FeatureBoxProps> = ({
  image,
  title,
  content,
  custom_style,
}) => {

  return (
    <Box position="relative" borderRadius="10px" style={custom_style}>
      <Box position="absolute" mt="-50px">
        <img width="170px" height="170px" src={image} alt="title" />
      </Box>

      <Box mt="150px"></Box>

      <Box color="white" whiteSpace="pre-line" fontSize="36px" fontWeight="900">
        {title}
      </Box>

      <Box mt="20px"></Box>

      <Box color="white" fontSize="18px" lineHeight="21px" fontWeight="300">
        {content}
      </Box>
    </Box>
  );
};

export default FeatureBox;
