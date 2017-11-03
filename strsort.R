strSort <- function(x, y = 1, z = " ", direction = "asc", cap = TRUE) {
  ## Sort a string 
  ## 1. from shortest word to longest (direction = "asc")
  ## 2. Longest to shortest (direction = "desc")
  ## 3. Capitalize (or not) cap = TRUE / FALSE
  a <- tolower(x[[y]]) %>% 
  strsplit(split = z) %>% unlist;
  if (direction == "asc") {
    a <- a[order(nchar(a))]; 
  } else {
    a <- a[order(-nchar(a))]; 
  }
  if (cap == TRUE) {
    a <- paste(toupper(substring(a, 1, 1)), 
               substring(a, 2), sep = "", collapse = z)
  } else {
    a <- paste(a, sep = "", collapse = z)
  }
}

strSortFrame <- function(x,col, separator, direction, cap) {
  tmp <- apply(x, 1, function(x,col, separator, direction, cap) 
    strSort(x,col,separator, direction, cap), col, separator, direction, cap)
}


