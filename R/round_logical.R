#' Logical rounding
#'
#' Always round fives up
#'
#' @param x numeric; see [base::round()]
#' @param digits numeric; see [base::round()]
#'
#' @details
#' R's default rounding function uses the IEC 60559 standard ('go to the even
#' digit'; see [base::round()] for more details). While this is useful for all
#' of the reasons outlined in the standard, sometimes AMLR scientists want to
#' reproduce 'logical' or 'traditional' rounding, meaning rounding up from 5.
#'
#' This function is implemented from \url{https://stackoverflow.com/a/12688836},
#' and is the same as the \code{janitor} function
#' \code{round_half_up} (see web link below).
#'
#' @seealso \url{https://sfirke.github.io/janitor/reference/round_half_up.html},
#' \url{https://appsilon.com/rounding-issues-in-r-julia-and-python/}
#'
#' @export
round_logical <- function(x, digits) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}
