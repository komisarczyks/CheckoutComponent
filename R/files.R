#' Validate input file with products
#'
#' Function verifies the content of a file which should contain a list of products.
#' It verifies if the file contains 3 rqeauired columns ('Item', 'Price' and 'SpecialPrice'),
#' if the Price column has numeric values and if the format of SpecialPrice column is correct.
#' @param file path to the file with products list
#' @return Function returns a list with two elements. Frst element is a logical variable with
#' the result of validation. If the validation fails the second element on the list is the message
#' describing what was wrong. If the validation was successfull then the second element will be
#' a data.frame with products read from file.
#' @export
validateProducts <- function(file="products.csv")
{
  if(file.exists(file))
    products <- read.csv(file, header=T, sep=",", stringsAsFactors=F)
  else
    return(list(FALSE, paste("File", file, "does not exist.")))

  if(all(c("Item", "Price", "SpecialPrice") %in% colnames(products)) == FALSE)
    return(list(FALSE, "Invalid format of input file with products. The file should have 3 columns: 'Item', 'Price' and 'SpecialPrice'."))
  if(all(is.numeric(products[, "Price"])) == FALSE)
    return(list(FALSE, "Invalid format of input file with products. The 'Price' column should contain numeric values."))
  if(all(grepl(pattern=" for ", x=products[products[, "SpecialPrice"] != "", "SpecialPrice"], fixed=T)) == FALSE)   # check if all non-empty values from SpecialPrice column contain a string " for "
    return(list(FALSE, "Invalid format of input file with products. The 'SpecialPrice' column should contain a string with format 'X for Y'."))
  return(list(TRUE, products))
}

#' Read products from file
#'
#' Function runs a validation of a supplied file containing products and throws an error
#' if it was not successfull.
#' If the file format was correct then the 'SpecialPrice' column is split into two seperate columns
#' named 'DiscountItemCount' (amount of items required to apply a special price) and DiscountPrice
#' (new price which can be applied if enough items are bought).
#' @inheritParams validateProducts
#' @return Function returns a data.frame with products read from file if the validation
#' was successfull.
#' @export
readProducts <- function(file="products.csv")
{
  valid <- validateProducts(file=file)
  if(valid[[1]] == FALSE)
    stop(valid[[2]])
  products <- valid[[2]]
  discount <- strsplit(products[, "SpecialPrice"], split=" for ")
  products[, "DiscountItemCount"] <- sapply(discount, "[", 1)
  products[, "DiscountPrice"] <- sapply(discount, "[", 2)
  return(products)
}

#' Validate input file with discounts
#'
#' Function verifies the content of a file which should contain a list of discounts
#' applicable when buying two different products.
#' It verifies if the file contains 3 reqauired columns ('Item1', 'Item2' and 'Discount')
#' and if the 'Discount' column has numeric values.
#' @param file path to the file with discount list for buying a pair of products.
#' @return Function returns a list with two elements. Frst element is a logical variable with
#' the result of validation. If the validation fails the second element on the list is the message
#' describing what was wrong. If the validation was successfull then the second element will be
#' a data.frame with discounts read from file.
#' @export
validateDiscounts <- function(products, file="discounts")
{
  if(file.exists(file))
    discounts <- read.csv(file, header=T, sep=",", stringsAsFactors=F)
  else
    return(list(FALSE, paste("File", file, "does not exist.")))
  if(all(c("Item1", "Item2", "Discount") %in% colnames(discounts)) == FALSE)
    return(list(FALSE, "Invalid format of input file containing pairs of items with discounts. The file should have 3 columns: 'Item1', 'Item2' and 'Discount'."))
  if(all(is.numeric(discounts[, "Discount"])) == FALSE)
    return(list(FALSE, "Invalid format of input file containing pairs of items with discounts. The 'Discount' column should contain numeric values."))
  return(list(TRUE, discounts))
}

#' Read discounts from file
#'
#' Function runs a validation of a supplied file containing discounts and throws an error
#' if it was not successfull.
#' @inheritParams validateProducts
#' @return Function returns a data.frame with discounts read from file if the validation
#' was successfull.
#' @export
readDiscounts <- function(products, file="discounts.csv")
{
  valid <- validateDiscounts(products=products, file=file)
  if(valid[[1]] == FALSE)
    stop(valid[[2]])
  discounts <- valid[[2]]
  return(discounts)
}


#' Validate input file with items for purchase
#'
#' Function verifies the content of a file which should contain a list of purchased items.
#' It verifies if the file contains 2 reqauired columns ('Item', 'Count')
#' and if the 'Count' column has numeric values.
#' @param file path to the file with items to be purchased.
#' @return Function returns a list with two elements. Frst element is a logical variable with
#' the result of validation. If the validation fails the second element on the list is the message
#' describing what was wrong. If the validation was successfull then the second element will be
#' a data.frame with items to be purchased.
#' @export
validatePurchaseItems <- function(products, file="discounts")
{
  if(file.exists(file))
    items <- read.csv(file, header=T, sep=",", stringsAsFactors=F)
  else
    return(list(FALSE, paste("File", file, "does not exist.")))
  if(all(c("Item", "Count") %in% colnames(items)) == FALSE)
    return(list(FALSE, "Invalid format of input file containing items for purchase. The file should have 3 columns: 'Item1', 'Item2' and 'Discount'."))
  if(all(is.numeric(items[, "Count"])) == FALSE)
    return(list(FALSE, "Invalid format of input file containing items for purchase. The 'Count' column should contain numeric values."))
  return(list(TRUE, items))
}

#' Read items for purchase from file
#'
#' Function runs a validation of a supplied file containing items to be purchased and throws an error
#' if it was not successfull.
#' @inheritParams validateProducts
#' @return Function returns a data.frame with items por purchase read from file if the validation
#' was successfull.
#' @export
readPurchaseItems <- function(products, file="purchaseItems.csv")
{
  valid <- validatePurchaseItems(products=products, file=file)
  if(valid[[1]] == FALSE)
    stop(valid[[2]])
  items <- valid[[2]]
  return(items)
}
