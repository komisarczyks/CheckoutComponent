#' @export
mainCheckoutComponent <- function(productsFile="products.csv", purchaseFile="purchaseItems.csv", discountsFile="pairDiscounts.csv")
{
  products <- readProducts(productsFile)
  items <- readPurchaseItems(purchaseFile, products)
  pairDiscounts <- readPairDiscounts(discountsFile, products)

  itemCost <- NULL
  for(i in 1:nrow(items))
    itemCost <- rbind(itemCost, calculateSingleItemCost(items[i, "Item"], items[i, "Count"], products))
  itemCost <- rbind(itemCost, calculatePairDiscount(items, pairDiscounts))
  print(paste0("Total cost: ", sum(itemCost[, "Cost"])))
  return(itemCost)
}
