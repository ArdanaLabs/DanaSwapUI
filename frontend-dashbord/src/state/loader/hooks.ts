import { useDispatch, useSelector } from 'react-redux'
import { AppDispatch, AppState } from 'state'
import { setLoading as _setLoading } from './actions'

export function useLoading () {
  const dispatch = useDispatch<AppDispatch>()
  const { loading } = useSelector<AppState, AppState['loader']>(
    state => state.loader
  )

  const setLoading = (loading: boolean) => dispatch(_setLoading(loading))

  return { loading, setLoading }
}
