# Written by: Emil Bach; 13-05-2020; version: 1.0
# Dependencies: base.
# This function takes any vector or matrix like object and splits it into chunks with a specified number of
# elements in them. Matrix like objects are split row-wise. Split elements are output in a list.
# Function inputs:
#   1. x, is a vector or matrix like object.
#   2. n, is an integer of length 1 indicating the desired number of elements in each chunk.


#Function to split objects into elements of size n - data.frame/matrix are split rowwise
split_length <- function(x, n){
  if(inherits(x, c("numeric", "integer", "character", "factor", "list", "IRanges", "XStringSet"))){
    x_size <- length(x)
  } else if(inherits(x, c("matrix", "data.frame"))){
    x_size <- nrow(x)
  }
  split(x, ceiling(seq(x_size)/n))
}
