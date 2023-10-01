#' Extracting a data portion
#'
#' @description
#' extract a portion of data saved as a \code{vector}, \code{matrix},
#' \code{data.frame}, or \code{list}
#'
#' @param x
#' an object to be portioned
#'
#' @param proportion
#' a \code{numeric} between 0 and 1, the relative portion size
#'
#' @param how
#' a \code{character}, specifying the portion method, one of:
#' - \code{"random"} (default), portion at random
#' - \code{"first"}, portion to the first elements
#' - \code{"last"}, portion to the last elements
#' - \code{"similar"}, portion to similar elements based on clustering
#' - \code{"dissimilar"}, portion to dissimilar elements based on clustering
#'
#' @param byrow
#' \code{TRUE} to portion row-wise or \code{FALSE} to portion column-wise
#'
#' @param ignore
#' (relevant if \code{how} is \code{"similar} or \code{"dissimilar)})
#' an \code{integer} vector of row indices (or column indices if
#' \code{byrow = FALSE}) to ignore during clustering
#'
#' @param cluster
#' (relevant if \code{how} is \code{"similar} or \code{"dissimilar)})
#' passed on to \code{\link[stats]{kmeans}}
#'
#' @param ...
#' further arguments to be passed to or from other methods
#'
#' @return
#' the portioned input \code{x} with the (row, column, ...) indices used
#' added as attributes \code{"indices"}
#'
#' @export

portion <- function(x, proportion, how, ...) {

}

#' @export
#' @rdname portion

portion.numeric <- function(x, proportion, how, ...) {

}

#' @export
#' @rdname portion

portion.matrix <- function(x, proportion, how, byrow, ignore, cluster, ...) {

}

#' @export
#' @rdname portion

portion.data.frame <- function(x, proportion, how, byrow, ignore, cluster, ...) {

}

#' @export
#' @rdname portion

portion.list <- function(x, proportion, how, ...) {

}
