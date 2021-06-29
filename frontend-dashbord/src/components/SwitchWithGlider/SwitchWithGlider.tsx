import React from 'react';
import { Box } from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';

const useStyles = makeStyles(({ palette }) => ({
  container: {
    display: 'flex',
    border: palette.background.paper,
    width: '100%',
    height: '100%',
    justifyContent: 'space-between'
  },
  activeItem: {
    borderRadius: '100px 0 0 100px',
    backgroundColor: palette.background.paper,
    transition: 'background .3s ease-in',
  }

  // glider: {
  //   transition: 'all 0.4s ease-out',
  //   position: 'absolute',
  //   borderRadius: '10px',
  //   backgroundColor: palette.primary.dark
  // }
}));

export interface SwitchWithGliderProps {
  elements: any;
  defaultIndex: number;
  gliderHeight: number;
  gliderWidth: number | string;
  marginBetweenSwitches: number;
  verticalGlider?: boolean;
}

const SwitchWithGlider: React.FC<SwitchWithGliderProps> = ({
  elements,
  gliderHeight,
  gliderWidth,
  defaultIndex,
  marginBetweenSwitches,
  verticalGlider
}) => {
  const classes = useStyles();
  // const [gliderPosition, setGliderPosition] = React.useState<any>(0);

  // React.useEffect(() => {
  //   if (!verticalGlider) {
  //     const incrementalDistance =
  //       (typeof gliderWidth === 'number' ? gliderWidth : 0) +
  //       marginBetweenSwitches;
  //     const newPosition = defaultIndex * incrementalDistance;
  //     setGliderPosition(newPosition);
  //   } else {
  //     const incrementalDistance = gliderHeight + marginBetweenSwitches;
  //     const newPosition = defaultIndex * incrementalDistance;
  //     setGliderPosition(newPosition);
  //   }
  // }, [
  //   defaultIndex,
  //   gliderHeight,
  //   gliderWidth,
  //   marginBetweenSwitches,
  //   verticalGlider
  // ]);

  const wrappedElements = elements.map((item: React.FC, index: number) =>
    index !== defaultIndex ? (
      <Box key={index}>{item}</Box>
    ) : (
      <Box className={classes.activeItem} key={index}>
        {item}
      </Box>
    )
  );

  return (
    <Box width="100%" height="100%">
      {/* <Box
        className={classes.glider}
        width={gliderWidth}
        height={gliderHeight}
        style={
          !verticalGlider
            ? {
                transform: `translateX(${gliderPosition}px)`,
                opacity: gliderPosition < 0 ? 0 : 1
              }
            : {
                transform: `translateY(${gliderPosition}px)`,
                borderRadius: '12px',
                opacity: gliderPosition < 0 ? 0 : 1
              }
        }
      /> */}
      <Box
        className={classes.container}
        flexDirection={!verticalGlider ? 'row' : 'column'}
        alignItems={!verticalGlider ? 'center' : 'flex-start'}
      >
        {wrappedElements}
      </Box>
    </Box>
  );
};

export default SwitchWithGlider;
