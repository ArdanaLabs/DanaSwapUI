import React from "react";
import {
  Box,
  useMediaQuery,
  Radio,
  RadioGroup,
  FormControlLabel,
  FormControl,
  FormLabel,
} from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useIsDarkMode } from "state/user/hooks";
import { Input } from "components/Input";

const useStyles = makeStyles(({ palette }) => ({
  title: {
    fontSize: "10px",
    color: palette.text.primary,
    fontWeight: "bold",
    marginBottom: "4px",
  },

  root: {
    "&:hover": {
      backgroundColor: "transparent",
    },
  },
  icon: {
    borderRadius: "50%",
    width: 18,
    height: 18,
    background: palette.type === "light" ? "#E9E9E9" : palette.common.white,
    transition: "background .3s ease-in",
    "input:hover ~ &": {
      // backgroundColor: "#ebf1f5",
    },
    "input:disabled ~ &": {
      boxShadow: "none",
      background: "rgba(206,217,224,.5)",
    },
  },
  checkedIcon: {
    background: palette.type === "light" ? palette.primary.main : palette.text.hint,
    // "&:before": {
    //   display: "block",
    //   width: 18,
    //   height: 18,
    //   backgroundImage: `url(${Check})`,
    //   backgroundRepeat: "no-repeat",
    //   backgroundPosition: "4px",
    //   content: '""',
    // },
    "input:hover ~ &": {
      // backgroundColor: "#206ba3",
    },
  },
  itemLabel: {
    fontSize: "10px",
    color: palette.text.secondary,
  },
  formControl: {
    "& .MuiRadio-root": {
      padding: "5px 9px",
    },
  },
  customInput: {
    fontSize: "7px",
    width: "calc(100% - 20px)",
    padding: "4px 2px 4px 9px",
    background: palette.type === 'light' ? '#E5E5E5' : "white",
  },
}));

export interface CustomRadioProps {
  option: any;
  value: any;
  customValue?: any;
}

const CustomRadio: React.FC<CustomRadioProps> = ({
  option,
  value,
  customValue = "",
}) => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  const [val, setVal] = React.useState(value);
  const [customVal, setCustomVal] = React.useState(customValue);

  const onCustomInputChange = (e: any) => {
    setCustomVal(e.target.value);
  };
  const StyledRadio = (props: any) => {
    return (
      <Radio
        className={classes.root}
        disableRipple
        color="default"
        checkedIcon={<span className={cx(classes.icon, classes.checkedIcon)} />}
        icon={<span className={classes.icon} />}
        {...props}
      />
    );
  };

  const handleChange = (event: any) => {
    // console.log(event.target);
    setVal(event.target.value);
  };

  return (
    <FormControl>
      <FormLabel className={cx(classes.title)}>{option.title}</FormLabel>
      <RadioGroup
        aria-label={option.title}
        name={option.title}
        value={val}
        onChange={handleChange}
      >
        {option.data.map((item: any, i: any) => (
          <Box key={i}>
            <FormControlLabel
              className={cx(classes.formControl)}
              value={item.value}
              control={<StyledRadio />}
              label={
                item.hasInput ? (
                  <Input
                    placeholder={"Custom Amount"}
                    value={customVal}
                    className={cx(classes.customInput)}
                    onChange={onCustomInputChange}
                    type={"number"}
                  />
                ) : (
                  <div className={cx(classes.itemLabel)}>{item.label}</div>
                )
              }
            />
          </Box>
        ))}
      </RadioGroup>
    </FormControl>
  );
};

export default CustomRadio;
