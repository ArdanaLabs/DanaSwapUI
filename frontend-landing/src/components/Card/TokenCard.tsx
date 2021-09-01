import React from "react";
import { Box, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    borderRadius: '30px',
    padding: '20px',
    color: '#FFFFFF',
    position: 'relative',
  },
  typographyPrimary: {
    fontFamily: 'Brandon Grotesque',
    fontStyle: 'normal',
    fontWeight: 900,
  },
  typographySecondary: {
    fontFamily: 'Museo Sans',
    fontStyle: 'normal',
    fontWeight: 'normal',
  },
  label: {
    fontSize: '20px',
    lineHeight: '115%',
    marginTop: '5px',
  },
  name: {
    fontSize: '50px',
    lineHeight: '110%',
    marginTop: '25px',
    marginBottom: '55px',
  },
  status: {
    fontSize: '15px',
    lineHeight: '115%',
  },
  image: {
    position: 'absolute',
    right: '-30px',
    top: '-70px',
  }
}));

export interface TokenCardProps {
  image: string;
  label: string;
  name: string;
  stabilityFee: number;
  ratio: number;
  background: string;
}

const TokenCard: React.FC<TokenCardProps> = ({
  image,
  label,
  name,
  stabilityFee,
  ratio,
  background
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)} style={{background: background}}>
      <Box className={cx(classes.image)}>
        <img src={image} alt='token' />
      </Box>

      <Box className={cx(classes.label, classes.typographyPrimary)} component={'p'}>{label}</Box>

      <Box className={cx(classes.name, classes.typographyPrimary)} component={'p'}>{name}</Box>

      <Box display='flex' justifyContent='space-between' className={cx(classes.typographySecondary, classes.status)}>
        <Box>{`Stability Fee  ${stabilityFee.toFixed(2)}%`}</Box>
        <Box>{`Min Coll. Ratio  ${ratio.toFixed(0)}%`}</Box>
      </Box>

    </Box>
  );
};

export default TokenCard;
