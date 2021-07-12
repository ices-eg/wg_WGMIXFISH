
nameChange <- function(x, form="new"){
  xn <- x
  if(form=="new"){
    xn <- gsub("-", "_dash_", xn, fixed = TRUE)
    xn <- gsub("<", "_lthan_", xn, fixed = TRUE)
    xn <- gsub(">", "_gthan_", xn, fixed = TRUE)
    xn <- gsub("=", "_equal_", xn, fixed = TRUE)
    xn <- gsub(".", "_dot_", xn, fixed = TRUE)
  }
  if(form=="old"){
    xn <- gsub("_dash_", "-", xn, fixed = TRUE)
    xn <- gsub("_lthan_", "<", xn, fixed = TRUE)
    xn <- gsub("_gthan_", ">", xn, fixed = TRUE)
    xn <- gsub("_equal_", "=", xn, fixed = TRUE)
    xn <- xn <- gsub("_dot_", ".", xn, fixed = TRUE)
  }
  return(xn)
}

# xnew <- nameChange(x)
# xnew
# xold <- nameChange(xnew, form = "old")
# xold
