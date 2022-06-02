# Written by: Emil Bach; 15-12-2020; version: 1.0
# Dependencies: base.
# This function takes any vector or matrix like object and splits it into a list of a specified length.
# Sizes of the chunks/pieces are about equal.
# Function inputs:
  # x, is any vector/matrix like object
  # n, is an integer of length 1, giving the desired number of chunks (length of the resulting list)
# Note: Matrix like objects are split row-wise.

#Function to chunk vector-like and matrix/data.frame objects into n pieces of about equal size
split_n <- function(x,n){
  if(inherits(x, c("numeric", "integer", "character", "factor", "list", "IRanges", "XStringSet"))){
    x_size <- length(x)
  } else if(inherits(x, c("matrix", "data.frame"))){
    x_size <- nrow(x)
  }
  split(x, rep_len(seq(n), x_size))
}
