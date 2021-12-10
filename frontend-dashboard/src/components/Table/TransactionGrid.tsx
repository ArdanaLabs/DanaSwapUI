import React, { useState } from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"
import { Button } from "components/Button"
import {
  DataGrid,
  GridColDef,
  GridValueGetterParams,
} from "@material-ui/data-grid"

const FILTER_ALL = 0
const FILTER_SWAP = 1
const FILTER_ADD = 2
const FILTER_REMOVE = 3

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  panel: {
    background: palette.background.paper,
    borderRadius: "10px",
    padding: "12px",
    height: "400px",
  },
  filter: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    margin: "20px 0",

    [breakpoints.down("xs")]: {
      "flexDirection": "column",
      "& > div": {
        width: "100%",
      },
    },
  },

  filterText: {
    "background":
      palette.type === "light"
        ? "#E7E7E7"
        : "linear-gradient(90deg, #3142A3 0%, #243183 100%)",
    "boxShadow":
      palette.type === "light"
        ? "unset"
        : "2px 2px 10px rgba(0, 0, 0, 0.1), inset 2px 2px 10px rgba(0, 0, 0, 0.1)",
    "fontSize": "11px",
    "fontWeight": 600,
    "lineHeight": "100%",
    "width": "500px",
    "padding": "15px 30px",
    "borderRadius": "20px",
    "color": palette.secondary.main,

    "& ::placeholder": {
      color: palette.secondary.main,
    },

    [breakpoints.down("xs")]: {
      flexDirection: "column",
      width: "100%",
    },
  },
  filterType: {
    background: palette.secondary.dark,
    color: palette.common.white,
    padding: "10px 15px",
    fontSize: "11px",
    lineHeight: "100%",
    margin: "10px",
  },

  active: {
    // background: palette.primary.light
  },
}))

const TransactionGrid: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  // const columns = [
  //   '',
  //   'TOTAL VALUE',
  //   'TOKEN AMOUNT',
  //   'TOKEN AMOUNT',
  //   'ACCOUNT',
  //   'TIME'
  // ]
  // const rows = [
  //   {
  //     action: 'Add USDC and ETH',
  //     totalValue: '$1.98k',
  //     tokenAmountUSDC: '0.93K USDC',
  //     tokenAmount: '0.3 DANA',
  //     account: '0x362C...2f25',
  //     time: '47 minutes ago'
  //   },
  //   {
  //     action: 'Add USDC and ETH',
  //     totalValue: '$1.98k',
  //     tokenAmountUSDC: '0.93K USDC',
  //     tokenAmount: '0.3 DANA',
  //     account: '0x362C...2f25',
  //     time: '47 minutes ago'
  //   },
  //   {
  //     action: 'Add USDC and ETH',
  //     totalValue: '$1.98k',
  //     tokenAmountUSDC: '0.93K USDC',
  //     tokenAmount: '0.3 DANA',
  //     account: '0x362C...2f25',
  //     time: '47 minutes ago'
  //   },
  //   {
  //     action: 'Add USDC and ETH',
  //     totalValue: '$1.98k',
  //     tokenAmountUSDC: '0.93K USDC',
  //     tokenAmount: '0.3 DANA',
  //     account: '0x362C...2f25',
  //     time: '47 minutes ago'
  //   },
  //   {
  //     action: 'Add USDC and ETH',
  //     totalValue: '$1.98k',
  //     tokenAmountUSDC: '0.93K USDC',
  //     tokenAmount: '0.3 DANA',
  //     account: '0x362C...2f25',
  //     time: '47 minutes ago'
  //   },
  //   {
  //     action: 'Add USDC and ETH',
  //     totalValue: '$1.98k',
  //     tokenAmountUSDC: '0.93K USDC',
  //     tokenAmount: '0.3 DANA',
  //     account: '0x362C...2f25',
  //     time: '47 minutes ago'
  //   },
  //   {
  //     action: 'Add USDC and ETH',
  //     totalValue: '$1.98k',
  //     tokenAmountUSDC: '0.93K USDC',
  //     tokenAmount: '0.3 DANA',
  //     account: '0x362C...2f25',
  //     time: '47 minutes ago'
  //   }
  // ]

  const columns: GridColDef[] = [
    {
      field: "stats",
      headerName: "",
      width: 90,
      sortable: false,
    },
    {
      field: "navUSD",
      headerName: "Total Value",
      width: 150,
    },
    {
      field: "amount",
      headerName: "Token Amount",
      width: 150,
    },
    {
      field: "age",
      headerName: "Age",
      width: 110,
    },
    {
      field: "fullName",
      headerName: "Full name",
      width: 160,
      valueGetter: (params: GridValueGetterParams) =>
        `${params.getValue(params.id, "firstName") || ""} ${
          params.getValue(params.id, "lastName") || ""
        }`,
    },
  ]

  const rows = [
    { id: 1, lastName: "Snow", firstName: "Jon", age: 35 },
    { id: 2, lastName: "Lannister", firstName: "Cersei", age: 42 },
    { id: 3, lastName: "Lannister", firstName: "Jaime", age: 45 },
    { id: 4, lastName: "Stark", firstName: "Arya", age: 16 },
    { id: 5, lastName: "Targaryen", firstName: "Daenerys", age: null },
    { id: 6, lastName: "Melisandre", firstName: null, age: 150 },
    { id: 7, lastName: "Clifford", firstName: "Ferrara", age: 44 },
    { id: 8, lastName: "Frances", firstName: "Rossini", age: 36 },
    { id: 9, lastName: "Roxie", firstName: "Harvey", age: 65 },
  ]

  const [filter, setFilter] = useState({
    text: "",
    type: FILTER_ALL,
  })

  const onFilterChange = (event: any) => {
    setFilter({ ...filter, ...event })
  }

  return (
    <Box>
      <Box className={cx(classes.filter)}>
        <Box textAlign="center" mt={mobile ? "20px" : "0px"}>
          <Button
            variant="contained"
            onClick={() => {
              onFilterChange({ type: FILTER_ALL })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_ALL,
            })}
          >
            ALL
          </Button>
          <Button
            variant="contained"
            onClick={() => {
              onFilterChange({ type: FILTER_SWAP })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_SWAP,
            })}
          >
            SWAPS
          </Button>
          <Button
            variant="contained"
            onClick={() => {
              onFilterChange({ type: FILTER_ADD })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_ADD,
            })}
          >
            ADDS
          </Button>
          <Button
            variant="contained"
            onClick={() => {
              onFilterChange({ type: FILTER_REMOVE })
            }}
            className={cx(classes.filterType, {
              [classes.active]: filter.type === FILTER_REMOVE,
            })}
          >
            REMOVES
          </Button>
        </Box>
      </Box>
      <Box className={cx(classes.panel)}>
        <DataGrid
          rows={rows}
          columns={columns}
          pageSize={5}
          checkboxSelection={false}
          disableSelectionOnClick={true}
        />
      </Box>
    </Box>
  )
}

export default TransactionGrid
