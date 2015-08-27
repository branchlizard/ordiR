bub <- function(x, expand = 5) {
  return(expand * decostand(x, method = "range"))
}
