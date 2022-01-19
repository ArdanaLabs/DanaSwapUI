import { useCallback } from "react"
import { shallowEqual, useDispatch, useSelector } from "react-redux"

import { AppDispatch, AppState } from "state"

import { updateVaultsAction } from "./actions"
import { VaultState, VaultInfo } from "./types"
import { fetchVaultsApi } from "./apis"

export function useVault(): { vaults: VaultInfo[]; updateVaults: () => void } {
  const dispatch = useDispatch<AppDispatch>()

  const { vaults } = useSelector<AppState, VaultState>(
    (state) => state.vault,
    shallowEqual
  )

  const updateVaults = useCallback(async () => {
    const vaults = await fetchVaultsApi()
    dispatch(updateVaultsAction(vaults))
  }, [dispatch])

  return {
    vaults,
    updateVaults,
  }
}
