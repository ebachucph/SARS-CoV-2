# Written by: Emil Bach; 14-12-2020; version: 1.0
# Dependencies: base
# The function takes a vector of values, finds the unique entries, and combines them as a comma-separated vector
# of length 1.
# Can be used in a dplyr pipe with group_by and summarise.
# note: All new values are characters!

# Start of function code
unique_combine <- function(x){
  toString(unique(x))
}
