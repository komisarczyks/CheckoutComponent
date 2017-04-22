readProducts <- function(file="products.csv")
{
  products <- read.csv(file, header=T, sep=",", stringsAsFactors=F)

  if(all(c("Item", "Price", "SpecialPrice") %in% colnames(products)) == FALSE)
    stop("Invalid format of input file containing products. The file should have 3 columns: 'Item', 'Price' and 'SpecialPrice'.")
  discount <- strsplit(products[, "SpecialPrice"], split=" for ")
  products[, "DiscountItemCount"] <- sapply(discount, "[", 1)
  products[, "DiscountPrice"] <- sapply(discount, "[", 2)
  return(products)
}


readPurchaseItems <- function(file="purchaseItems.csv", products)
{
  items <- read.csv(file, header=T, sep=",", stringsAsFactors=F)
  if(all(c("Item", "Count") %in% colnames(items)) == FALSE)
    stop("Invalid format of input file containing purchased items. The file should have 2 columns: 'Item' and 'Count'.")
  if(any(items[, "Item"] %in% products[, "Item"] == FALSE))
    stop("Input file contains invalid product.")
  return(items)
}


readPairDiscounts <- function(file="pairDiscounts.csv", products)
{
  pairDiscounts <- read.csv(file, header=T, sep=",", stringsAsFactors=F)
  if(all(c("Item1", "Item2", "Discount") %in% colnames(pairDiscounts)) == FALSE)
    stop("Invalid format of input file containing pairs of items with discounts. The file should have 3 columns: 'Item1', 'Item2' and 'Discount'.")
  if(any(unlist(pairDiscounts[, c("Item1", "Item2")]) %in% products[, "Item"] == FALSE))
    stop("Input file contains invalid product.")
  return(pairDiscounts)
}
