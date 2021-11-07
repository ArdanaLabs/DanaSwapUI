export const currencyFormatter = (quantity: number, decimal: number = 6) => {
  return quantity / (10 ** decimal);
};
