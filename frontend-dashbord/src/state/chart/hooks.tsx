// import { useSelector } from "react-redux";
// import { AppState } from "state";
import { API_URL } from "config/endpoints";

export async function getAggVolume(start: string, end: string, grain: string) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/aggregate/volume?start=${start}&end=${end}&grain="${grain}"`
    );

    return result.json();
  } catch (e) {
    console.log("error fetching", e);
    return null;
  }
}

export async function getAggLiquidity(
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/aggregate/liquidity?start=${start}&end=${end}&grain="${grain}"`
    );

    return result.json();
  } catch (e) {
    console.log("error fetching", e);
    return null;
  }
}

export async function getPoolFees(
  pool: string,
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/pool/fees?pool=${pool}start=${start}&end=${end}&grain="${grain}"`
    );

    return result.json();
  } catch (e) {
    console.log("error fetching", e);
    return null;
  }
}

export async function getPoolVolume(
  pool: string,
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/pool/volume?pool=${pool}start=${start}&end=${end}&grain="${grain}"`
    );

    return result.json();
  } catch (e) {
    console.log("error fetching", e);
    return null;
  }
}

export async function getPoolLiquidity(
  pool: string,
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/pool/liquidity?pool=${pool}start=${start}&end=${end}&grain="${grain}"`
    );

    return result.json();
  } catch (e) {
    console.log("error fetching", e);
    return null;
  }
}

export async function getPoolTXCount(
  pool: string,
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/pool/tx-count?pool=${pool}start=${start}&end=${end}&grain="${grain}"`
    );

    return result.json();
  } catch (e) {
    console.log("error fetching", e);
    return null;
  }
}
