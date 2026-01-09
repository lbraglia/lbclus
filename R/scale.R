#'@export
scale_mad <- function(x, center = TRUE, scale = TRUE){
  if (center) x <- x - median(x, na.rm = TRUE)
  if (scale)  x <- x / mad(x, na.rm = TRUE)
  x
}
