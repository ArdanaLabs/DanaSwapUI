import { DataGrid } from "@mui/x-data-grid"
import { Theme } from "@mui/material"
import { withStyles } from "@mui/styles"
import { FontFamilies } from "theme"

const StyledDataGrid = withStyles((theme: Theme) => ({
  root: {
    border: "unset !important",
    [`& .MuiDataGrid-columnHeaderTitle`]: {
      fontFamily: FontFamilies.Brandon,
      fontStyle: "normal",
      fontWeight: 900,
      fontSize: "15px",
      color: theme.palette.primary.main,
      marginRight: "20px",

      [`& svg`]: {
        color: `${theme.palette.primary.main} !important`,
      },
    },
    [`& .MuiDataGrid-columnSeparator`]: {
      opacity: "0 !important",
      border: "unset",
    },
    [`& .MuiDataGrid-cell:focus, & .MuiDataGrid-columnHeader:focus`]: {
      outline: "none !important",
    },
    [`& .MuiDataGrid-columnHeader:focus-within, & .MuiDataGrid-cell:focus-within`]:
      {
        outline: "none !important",
      },
    [`& .MuiDataGrid-window, & .MuiDataGrid-viewport`]: {
      overflow: "unset !important",
    },
    [`& .MuiDataGrid-cell`]: {
      border: "unset !important",
    },
    [`& .MuiDataGrid-columnsContainer`]: {
      border: "unset !important",
    },
  },
  columnHeaders: {
    border: "unset !important",
  },
  row: {
    fontFamily: FontFamilies.Museo,
    fontStyle: "normal",
    fontWeight: "normal",
    fontSize: "16px",
    color: theme.palette.primary.main,
    borderBottom: `1px solid ${
      theme.palette.mode === "dark" ? "#72D2F388" : "#3651CD88"
    }`,
    width: "100% !important",
    padding: "0 5px",
    cursor: "pointer",
  },
  cell: {
    border: "unset !important",
  },
}))(DataGrid)

export default StyledDataGrid
