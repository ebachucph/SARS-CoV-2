pairwise_jaccard <- function(l){
  if(is.null(names(l))){
    combinations <- combn(seq(length(l)), 2)
  } else {
    combinations <- combn(names(l), 2, simplify = F)
  }
  
  do.call("rbind",lapply(combinations, function(i){
    A <- l[[i[1]]]
    B <- l[[i[2]]]
    data.frame(Row = i[1], Col = i[2], Distance = 1 - length(intersect(A, B)) / length(union(A,B)))
  }))
}