#Rewritten ecdf to allow for other ecdf methods than '<='
my_ecdf <- function (x, type = ">=", scaled = T)
{
  if(!grepl("[<=>]", type))
    stop("'type' must contain '<', '=' '>'")
  x <- sort(x)
  n <- length(x)
  if (n < 1) 
    stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  rval <- approxfun(vals, {
    temp <- colSums(Vectorize(function(a, b, gle){gle <- match.fun(gle); gle(a, b)}, "b")(x, vals, type))/n
    if(scaled) temp/max(temp) else temp 
    }, method = "constant", rule = 2, f = 0, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}
