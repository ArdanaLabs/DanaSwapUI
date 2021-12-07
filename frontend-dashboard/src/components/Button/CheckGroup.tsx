import React, { useEffect, useState } from "react";
import { makeStyles, Theme, createStyles } from "@material-ui/core/styles";
import FormLabel from "@material-ui/core/FormLabel";
import FormControl from "@material-ui/core/FormControl";
import FormGroup from "@material-ui/core/FormGroup";
import FormControlLabel from "@material-ui/core/FormControlLabel";
import Checkbox, { CheckboxProps } from "@material-ui/core/Checkbox";
import { Box, Grid } from "@material-ui/core";
import cx from "classnames";

const useStyles = makeStyles((theme: Theme) =>
  createStyles({
    root: {
      "&:hover": {
        backgroundColor: "transparent",
      },

      "& .MuiFormLabel-root": {
        fontStyle: "normal",
        fontWeight: 900,
        fontSize: "14px",
        lineHeight: "16px",
        color: theme.palette.secondary.main,
        marginBottom: "30px",
      },

      "& .MuiTypography-body1": {
        fontStyle: "normal",
        fontWeight: 500,
        fontSize: "10px",
        lineHeight: "16px",        
        color: theme.palette.secondary.main,
      },
    },
  })
);
const useCheckBoxStyles = makeStyles((theme: Theme) =>
  createStyles({
    root: {
      "&:hover": {
        backgroundColor: "transparent",
      },
    },
    icon: {
      borderRadius: "50%",
      width: 18,
      height: 18,
      boxShadow:
        "inset 0 0 0 1px rgba(16,22,26,.2), inset 0 -1px 0 rgba(16,22,26,.1)",
      backgroundColor: theme.palette.type === "light" ? "#E9E9E9" : "#FFFFFF",
      "$root.Mui-focusVisible &": {
        outline: "2px auto rgba(19,124,189,.6)",
        outlineOffset: 2,
      },
      "input:hover ~ &": {
        backgroundColor: "#ebf1f5",
      },
      "input:disabled ~ &": {
        boxShadow: "none",
        background: "rgba(206,217,224,.5)",
      },
    },
    checkedIcon: {
      backgroundColor: "#235DF4",
      "&:before": {
        display: "block",
        width: 18,
        height: 18,
        backgroundImage:
          "url(\"data:image/svg+xml;charset=utf-8,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'%3E%3Cpath" +
          " fill-rule='evenodd' clip-rule='evenodd' d='M12 5c-.28 0-.53.11-.71.29L7 9.59l-2.29-2.3a1.003 " +
          "1.003 0 00-1.42 1.42l3 3c.18.18.43.29.71.29s.53-.11.71-.29l5-5A1.003 1.003 0 0012 5z' fill='%23fff'/%3E%3C/svg%3E\")",
        content: '""',
      },
      "input:hover ~ &": {
        backgroundColor: "#106ba3",
      },
    },
  })
);

function StyledCheckbox(props: CheckboxProps) {
  const classes = useCheckBoxStyles();

  return (
    <Checkbox
      className={classes.root}
      disableRipple
      color="default"
      checkedIcon={<span className={cx(classes.icon, classes.checkedIcon)} />}
      icon={<span className={classes.icon} />}
      inputProps={{ "aria-label": "decorative checkbox" }}
      {...props}
    />
  );
}

export interface CheckGroupProps {
  list: any;
}

export interface CheckStateProps {
  [x: string]: boolean;
}

const CheckGroup: React.FC<CheckGroupProps> = ({ list }) => {
  const classes = useStyles();
  const [state, setState] = useState<CheckStateProps[]>([]);

  useEffect(() => {
    let newState = list.map((item: string) => {
      return { [item]: false };
    });

    setState(newState);
    // eslint-disable-next-line
  }, [list]);

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    let updatedState: any = state.map((item: any) => {
      if (Object.keys(item)[0] === event.target.name) {
        return { [event.target.name]: event.target.checked };
      }
      return {
        [Object.keys(item)[0]]: Object.values(item)[0],
      };
    });
    setState(updatedState);
  };

  return (
    <Box className={cx(classes.root)}>
      <FormControl component="fieldset">
        <FormLabel component="legend">Select pools where you want to deposit:</FormLabel>
        <FormGroup>
          <Grid container>
            {state.map((item, index) => (
              <Grid item xs={6} key={index}>
                <FormControlLabel
                  control={
                    <StyledCheckbox
                      checked={Object.values(item)[0]}
                      onChange={handleChange}
                      name={Object.keys(item)[0]}
                    />
                  }
                  label={Object.keys(item)[0]}
                />
              </Grid>
            ))}
          </Grid>
        </FormGroup>
      </FormControl>
    </Box>
  );
};
export default CheckGroup;
