import { withStyles } from "@material-ui/core/styles";
import { DataGrid } from "@material-ui/data-grid";

const StyledDataGrid = withStyles((theme) => ({
  root: {
    border: "unset",
    "& .MuiDataGrid-columnHeaderTitle": {
      fontFamily: "Brandon Grotesque",
      fontStyle: "normal",
      fontWeight: 900,
      fontSize: "15px",
      color: theme.palette.primary.main,
      marginRight: "20px",

      "& svg": {
        color: `${theme.palette.primary.main} !important`,
      },
    },
    "& .MuiDataGrid-columnSeparator": {
      opacity: "0 !important",
      border: "unset",
    },
    "& .MuiDataGrid-cell:focus, & .MuiDataGrid-columnHeader:focus": {
      outline: "none !important",
    },
    "& .MuiDataGrid-columnHeader:focus-within, & .MuiDataGrid-cell:focus-within":
      {
        outline: "none !important",
      },
    "& .MuiDataGrid-window, & .MuiDataGrid-viewport": {
      overflow: "unset !important",
    },
    "& .MuiDataGrid-cell": {
      border: "unset !important",
    },
    "& .MuiDataGrid-columnsContainer": {
      border: "unset !important",
    },
  },
  columnHeader: {},
  row: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: "normal",
    fontSize: "16px",
    color: theme.palette.primary.main,
    borderBottom: `1px solid ${
      theme.palette.type === "dark" ? "#72D2F3" : "#3651CD"
    }`,
    marginBottom: "15px",
    width: "100% !important",
    padding: "0 5px",
    cursor: "pointer",
  },
  cell: {
    border: "unset !important",
  },
}))(DataGrid);

export default StyledDataGrid;
