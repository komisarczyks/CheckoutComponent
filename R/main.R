#' Checkout Component
#'
#' Main function of the package. It takes three files as input patameters. Files should contain
#' a list of available products with their regular and special prices (\code{productsFile}),
#' a list of discounts for buying a bundle of two different products (\code{discountsFile})
#' and a list of items which a user wants to purchase (\code{purchaseFile}. Function reads all
#' the input files and calculates the final cost vaue including discounts from special prices
#' and from bundles of two different products.
#' @param productsFile path to a file with a list of products. File should have 3 columns:
#' 'Item', 'Price' and 'SpecialPrice'
#' @param purchaseFile path to a file with a list of items to be urchased. File should have
#' 2 columns: 'Item' and 'Count'
#' @param discountsFile path to a file with a list of discounts for purchasing two different
#' products together. File should have 3 columns: 'Item1', 'Item2' and 'Discount'
#' @return Function returns a data.frame with 4 columns: "Item", "Count", "Discount"
#' and "Cost". Each row represents either a seperate product from the list of purchased items
#' with its regular or special price or an additional discount from purchasing a bundle of two
#' products .
#' @export
mainCheckoutComponent <- function(productsFile="products.csv", purchaseFile="purchaseItems.csv", discountsFile="discounts.csv")
{
  products <- readProducts(file=productsFile)
  items <- readPurchaseItems(products=products, file=purchaseFile)
  discounts <- readDiscounts(products=products, file=discountsFile)

  costTab <- NULL
  for(i in 1:nrow(items))
    costTab <- rbind(costTab, calculateSingleItemCost(itemName=items[i, "Item"], itemCount=items[i, "Count"], products=products))
  costTab <- rbind(costTab, calculateDiscount(items=items, discounts=discounts))
  printReceipt(costTab=costTab, decimal=TRUE)
  return(costTab)
}

#' Total cost calculation
#'
#' Function calculates total cost from a data.frame containing purchased items.
#' @param costTab a data.frame with a list of purchased items. Requires a column called 'Cost'
#' @param decimal if TRUE then function will format cost value to have 2 decimal places.
#' @return Function returns total cost of purchased items as numeric (if \code{decimal} is FALSE)
#' or as character string (when \code{decimal} is TRUE)
#' @export
getTotalCost <- function(costTab, decimal=TRUE)
{
  totalCost <- sum(as.numeric(costTab[, "Cost"]))
  if(decimal)
    totalCost <- format(round(x=totalCost, digits=2), nsmall=2)
  return(totalCost)
}

#' Print receipt
#'
#' Function prints the final cost table (data.frame) and a cumulative cost including all discounts.
#' @inheritParams getTotalCost
#' @export
printReceipt <- function(costTab, decimal=TRUE)
{
  if(decimal)
    costTab[, "Cost"] <- format(round(x=costTab[, "Cost"], digits=2), nsmall=2)
  print(costTab, row.names=FALSE)
  print(paste("Total cost: ", getTotalCost(costTab=costTab, decimal=decimal)), quote=FALSE, row.names=F)
}
