export const extractXAxis = (arr: any[]): string[] => {
  const length = arr.length
  let newAxis: string[] = []

  newAxis.push(arr[0].start)
  for (let i = 0; i < length; i++) {
    newAxis.push(arr[i].end)
  }
  return newAxis
}

export const extractYAxis = (arr: any[], filter: string): any[] => {
  const length = arr.length
  let newAxis: any[] = []

  for (let i = 0; i < length; i++) {
    newAxis.push(arr[i][filter])
  }

  return newAxis
}
