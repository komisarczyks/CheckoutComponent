#' Single item cost
#'
#' Function calculates a cost for a specified amount of single item including
#' regular price and special price for buying a set amount of items
#' @param itemName name of an item for which the price will be calculated
#' @param itemCount total amount of item to purchase
#' @param products a data.frame contaning all available products with their regular
#' prices and special prices
#' @return Function returns a data.frame with 4 columns ("Item", "Count", "Discount"
#' and "Cost") and 1 or 2 rows (1 row if only regular or special price is used,
#' 2 rows if both of these prices are used)
#' @export
calculateSingleItemCost <- function(itemName, itemCount, products)
{
  if(itemName %in% products[, "Item"] == FALSE)
    stop(paste("Purchased item", itemName, "is not on the available product list."))
  itemCost <- NULL
  discountItemCount <- as.numeric(products[products[, "Item"] == itemName, "DiscountItemCount"])
  if(!is.na(discountItemCount))
  {
    discountPacks <- floor(itemCount / discountItemCount)
    if(discountPacks > 0)
      itemCost <- data.frame(Item=itemName, Count=discountPacks*discountItemCount, Discount=products[products[, "Item"] == itemName, "SpecialPrice"], Cost=discountPacks * as.numeric(products[products[, "Item"] == itemName, "DiscountPrice"]))
  }else{
    discountPacks <- 0
    discountItemCount <- 0
  }
  if(itemCount > discountItemCount * discountPacks)
  {
    remainingItemCount <- itemCount - discountPacks * discountItemCount
    itemCost <- rbind(itemCost, data.frame(Item=itemName, Count=remainingItemCount, Discount="", Cost=remainingItemCount * as.numeric(products[products[, "Item"] == itemName, "Price"])))
  }
  return(itemCost)
}


#' Calculate discount from bundles of products
#'
#' @param items data.frame with all items that are purchased. Contains 2 columns:
#' 'Item' with product name and 'Count' with the amount of items purchased
#' @param discounts data.frame with all discounts for bundle of two products
#' purchased together. Contains 3 columns: 'Item1', 'Item2' (product names) and Discount
#' (applied to every pair of two defined products).
#' @return Funcrion returns a data.frame with 4 columns ("Item", "Count", "Discount"
#' and "Cost" - same format as in \code{calculateSingleItemCost} function). Each row is
#' a seperate discount from a bundle of two products. The value in 'Discount' column is
#' created by combining the names of two products in a bundle.
#' @export
calculateDiscount <- function(items, discounts)
{
  discountTab <- NULL
  for(i in 1:nrow(Discounts))
  {
    item1 <- discounts[i, "Item1"]
    item2 <- discounts[i, "Item2"]
    discount <- discounts[i, "Discount"]
    if(item1 %in% items[, "Item"] && item2 %in% items[, "Item"])
    {
      bundleCount <- min(items[which(items[, "Item"] == item1), "Count"], items[which(items[, "Item"] == item2), "Count"])
      if(bundleCount > 0)
      {
        discountTotal <-  bundleCount * discounts[i, "Discount"] * (-1)
        discountTab <- rbind(discountTab, data.frame(Item="", Count=bundleCount, Discount=paste(item1, "+", item2), Cost=discountTotal))
      }
    }
  }
  return(discountTab)
}

