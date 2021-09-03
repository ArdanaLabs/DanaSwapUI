import { withStyles } from "@material-ui/core/styles";
import { DataGrid } from '@material-ui/data-grid';

const StyledDataGrid = withStyles(theme => ({
  root: {
    border: 'unset',
    '& .MuiDataGrid-columnHeaderTitle': {
      fontFamily: 'Brandon Grotesque',
      fontStyle: 'normal',
      fontWeight: 900,
      fontSize: '15px',
      color: theme.palette.primary.main
    },
    '& .MuiDataGrid-columnSeparator': {
      opacity: '0 !important',
    },

    '& .MuiDataGrid-columnsContainer': {
      border: 'unset',
    },

    '& .MuiDataGrid-cell:focus, & .MuiDataGrid-columnHeader:focus': {
      outline: 'none !important'
    },
    '& .MuiDataGrid-columnHeader:focus-within, & .MuiDataGrid-cell:focus-within':{
      outline: 'none !important'
    },

    '& .MuiDataGrid-renderingZone': {
      maxHeight: 'unset !important',
    },

    '& .MuiDataGrid-window': {
      overflowX: 'unset !important',
    }
  },
  columnHeader: {
  },
  row: {
    fontFamily: 'Museo Sans',
    fontStyle: 'normal',
    fontWeight: 'normal',
    fontSize: '18px',
    color: theme.palette.primary.main,
    borderRadius: '30px',
    border: `1px solid ${theme.palette.primary.main}88`,
    marginBottom: '15px',
    width: '100% !important',
    padding: '0 5px',
    cursor: 'pointer',
  },
  cell: {
    border: 'unset !important',
  }
}))(DataGrid);

export default StyledDataGrid;
