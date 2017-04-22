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

calculatePairDiscount <- function(items, pairDiscounts)
{
  discountTab <- NULL
  for(i in 1:nrow(pairDiscounts))
  {
    item1 <- pairDiscounts[i, "Item1"]
    item2 <- pairDiscounts[i, "Item2"]
    discount <- pairDiscounts[i, "Discount"]
    if(item1 %in% items[, "Item"] && item2 %in% items[, "Item"])
    {
      bundleCount <- min(items[which(items[, "Item"] == item1), "Count"], items[which(items[, "Item"] == item2), "Count"])
      discountTotal <-  bundleCount * pairDiscounts[i, "Discount"] * (-1)
      discountTab <- rbind(discountTab, data.frame(Item="", Count=bundleCount, Discount=paste(item1, "+", item2), Cost=discountTotal))
    }
  }
  return(discountTab)
}

