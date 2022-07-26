# Written by: Emil Bach; 15-12-2020; version: 1.0
# Dependencies: base.
# This function takes two strings of equal lengths and finds the letters at the indexes of potential mismathces.
# The letter(s) at mismatching indexes in the subject are returned.
# Mismatches are returned in a list with length == query|subject. Each list element contains a character vector
# with the letters at mismatching  positions in increasing order of their indexes.
# Function inputs:
#   1. query, character vector of minimum length 1
#   2. subject, character vector of minimum length 1
# Note: Arguments to "query" and "subject" must have equal lengths.

#Function to find and return the mismatching letters between 2 strings of equal lengths
mismatch_letters <- function(query, subject){
  #Check arguments
  stopifnot(
    `Argument "query" is missing, with no default` = !missing(query),
    `Argument "subject" is missing, with no default` = !missing(subject),
    `Argument "query" must be a character vector` = is.character(query),
    `Argument "subject" must be a character vector` = is.character(subject),
    `Arguments "query" and "subject" must have equal lengths` = length(query) == length(subject)
  )
  
  #Find mismatching letters
  mapply(function(x, y) {
    mm <- which(charToRaw(x) != charToRaw(y))
    if(length(mm) == 0){
      ""
    } else {
      substring(y, mm, mm)
    }
  },query, subject, USE.NAMES = FALSE, SIMPLIFY = F)
}
