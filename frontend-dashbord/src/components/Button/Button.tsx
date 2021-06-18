import { withStyles } from '@material-ui/core/styles'
import Button from '@material-ui/core/Button'

const StyledButton = withStyles({
    root: {
        padding: '4px 43px 7px 43px',
        borderRadius: '25px',
        backgroundColor: '#006DFF',
        '&:hover': {
          backgroundColor: '#1976d2'
        }
    },
  })(Button);


export default StyledButton
