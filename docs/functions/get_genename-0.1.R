# Simple function to extract gene name from uniprot accession (only tested on SARS2 proteins)

get_genename <- Vectorize(function(uniprot_acc){
  require(tidyverse)
  url <- paste0("https://www.uniprot.org/uniprot/", uniprot_acc, ".txt")
  out <- readLines(url) %>% 
    str_subset(., "^GN") %>% 
    str_extract(., "(?<=Name=|ORFNames=).*?(?= |;)") %>% 
    ifelse(str_detect(., "^[0-9]"), paste0("ORF", .), .)
}, vectorize.args = "uniprot_acc", SIMPLIFY = F)
