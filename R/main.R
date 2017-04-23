#' @export
mainCheckoutComponent <- function(productsFile="products.csv", purchaseFile="purchaseItems.csv", discountsFile="discounts.csv")
{
  products <- readProducts(file=productsFile)
  items <- readPurchaseItems(products=products, file=purchaseFile)
  discounts <- readDiscounts(products=products, file=discountsFile)

  itemCost <- NULL
  for(i in 1:nrow(items))
    itemCost <- rbind(itemCost, calculateSingleItemCost(itemName=items[i, "Item"], itemCount=items[i, "Count"], products=products))
  itemCost <- rbind(itemCost, calculateDiscount(items=items, discounts=discounts))
  print(paste0("Total cost: ", sum(itemCost[, "Cost"])))
  return(itemCost)
}
