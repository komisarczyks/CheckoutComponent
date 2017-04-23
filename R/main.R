#' @export
mainCheckoutComponent <- function(productsFile="products.csv", purchaseFile="purchaseItems.csv", discountsFile="discounts.csv")
{
  products <- readProducts(productsFile)
  items <- readPurchaseItems(purchaseFile, products)
  discounts <- readDiscounts(discountsFile, products)

  itemCost <- NULL
  for(i in 1:nrow(items))
    itemCost <- rbind(itemCost, calculateSingleItemCost(items[i, "Item"], items[i, "Count"], products))
  itemCost <- rbind(itemCost, calculateDiscount(items, discounts))
  print(paste0("Total cost: ", sum(itemCost[, "Cost"])))
  return(itemCost)
}
