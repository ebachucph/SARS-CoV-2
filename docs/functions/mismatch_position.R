# Written by: Emil Bach; 15-12-2020; version: 1.0
# Dependencies: base.
# This function takes two strings of equal lengths and finds the indices of potential mismathces.
# Mismatches are returned in a list with length == query|subject. Each list element contains an integer vector
# with positions of mismatches in increasing order.
# Function inputs:
#   1. query, character vector of minimum length 1
#   2. subject, character vector of minimum length 1
# Note: Arguments to "query" and "subject" must have equal lengths.

#Function to find and return the mismatching positions between 2 strings of equal lengths
mismatch_position <- function(query, subject){
  #Check arguments
  stopifnot(
    `Argument "query" is missing, with no default` = !missing(query),
    `Argument "subject" is missing, with no default` = !missing(subject),
    `Argument "query" must be a character vector` = is.character(query),
    `Argument "subject" must be a character vector` = is.character(subject),
    `Arguments "query" and "subject" must have equal lengths` = length(query) == length(subject)
  )
  
  #Find indices of mismatching letters
  mapply(function(x, y) {
    idx <- which(charToRaw(x) != charToRaw(y))
    if(length(idx) == 0) {0} else {idx}
    },
         query, subject, USE.NAMES = FALSE, SIMPLIFY = F)
}
