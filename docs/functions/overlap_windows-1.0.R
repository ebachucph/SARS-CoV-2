# Written by: Emil Bach; 13-05-2020; version: 1.0
# Dependencies: base, tidyverse, data.table, GenomicRanges, plyranges.
# Function to perform a function on the center index of overlapping indices in a data frame with start/end values.
# The data must have columns called seqnames, start, and end.
# Function inputs:
#   1. d, matrix or data frame like object - must have columns named "seqnames", "start", and "end".
#   2. minoverlap, an integer of length 1 indicating the minimum overlap needed for segments to be grouped together.
#   3. value_col, a character of length 1 indicating the name of column containing the values used for calculations
#   4. out_name, a character of length 1 indicating the name of the column with the results of the calculations.
#   5. FUN, a function to perform on the values from value_col of overlapping segments in d.

#Dependencies
###CRAN
require(tidyverse)
require(parallel)
require(data.table)

###Bioconductor
require(GenomicRanges)
require(plyranges)

#Function to find indices I overlapping index i in data, do FUN on their values, and return output to index i
overlap_windows <- function(d, minoverlap, value_col, out_name, FUN){
  tryCatch({
    #Transform d to GRanges object, r, and find overlaps with self
    r <- as_granges(d)
    ol <- findOverlaps(r,r,minoverlap = minoverlap)
    
    #Split each overlap group into seperate lists
    I_list <- split(subjectHits(ol),queryHits(ol))
    
    #Vectorize relevant columns from GRanges object
    starts <- start(r)
    ends <- end(r)
    vals <- mcols(r)[[value_col]]
    
    mcols(r) <- cbind(mcols(r), rbindlist(lapply(I_list, function(I){
      data.table(temp = FUN(vals[I]), 
                 window_start = min(starts[I]),
                 window_end = max(ends[I]),
                 window_n = length(I)
      )
    })
    )
    )
    as.data.table(r) %>% dplyr::rename({{ out_name }} := "temp")
  }, error = function(e){
    d_temp <- copy(d)
    cols_ <- c("start", "end", "width")[c("start", "end", "width") %in% names(d_temp)]
    for (j in cols_) {
      set(d_temp, j = j, value = ifelse(any(is.na(d_temp[[j]])), 0L, d_temp[[j]]))
    }
    out <- cbind(d_temp, d_temp[, .(temp = NA_real_, 
                                    window_start = NA_integer_,
                                    window_end = NA_integer_,
                                    window_n = 1L)]) %>% 
      as_granges(.) %>% 
      as.data.table(.) %>% 
      dplyr::rename({{ out_name }} := "temp")
    
    out[, `:=`(
      start = ifelse(any(is.na(d[, cols_, with = F])), NA_integer_, start),
      end = ifelse(any(is.na(d[, cols_, with = F])), NA_integer_, end)
    )
    ]
  }
  )
}
