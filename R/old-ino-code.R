# #' Helper function for subsetting
# #'
# #' @details
# #' This function is part of the \code{$argument(action = "subset")} method of a
# #' \code{\link{Nop}} object and subsets a given argument.
# #'
# #' @param argument
# #' A \code{vector}, \code{matrix}, or \code{data.frame}.
# #' @param byrow
# #' Only relevant if \code{argument} is a \code{matrix} or \code{data.frame}.
# #' In that case, either \code{TRUE} to subset row-wise (default) or
# #' \code{FALSE} to subset column-wise.
# #' @param how
# #' A \code{character}, specifying how to subset. Can be one of:
# #' - \code{"random"} (default), subset at random
# #' - \code{"first"}, subset to the first elements
# #' - \code{"last"}, subset to the last elements
# #' - \code{"similar"}, subset to similar elements
# #' - \code{"dissimilar"}, subset to dissimilar elements
# #' The options \code{"similar"} and \code{"dissimilar"} apply k-means
# #' clustering via \code{\link[stats]{kmeans}} and require that
# #' the argument \code{argument_name} is \code{numeric}.
# #' @param proportion
# #' A \code{numeric} between \code{0} and \code{1}, specifying the
# #' subset proportion.
# #' By default, \code{proportion = 0.5}.
# #' @param centers
# #' Only relevant, if \code{how = "(dis)similar"}.
# #' In that case, passed to \code{\link[stats]{kmeans}}.
# #' By default, \code{centers = 2}.
# #' @param ignore
# #' Only relevant if \code{argument} is a \code{matrix} or \code{data.frame} and
# #' \code{how = "(dis)similar"}.
# #' In that case, a \code{integer} (vector) of row indices (or column indices
# #' if \code{byrow = FALSE}) to ignore for clustering.
# #'
# #' @return
# #' The subsetted input \code{argument}.
# #'
# #' @keywords internal

# helper_subset <- function(
#     argument, byrow = TRUE, how = "random", proportion = 0.5, centers = 2,
#     ignore = integer()
# ) {
#   how <- match_arg(
#     how, c("random", "first", "last", "similar", "dissimilar")
#   )
#   checkmate::assert_number(proportion, lower = 0, upper = 1)
#   df_flag <- ifelse(is.data.frame(argument), TRUE, FALSE)
#   if (is.atomic(argument) && is.null(dim(argument)) && length(argument) > 1) {
#     argument <- as.data.frame(argument)
#     vector_flag <- TRUE
#     byrow <- TRUE
#     ignore <- integer()
#   } else if (is.data.frame(argument) || is.matrix(argument)) {
#     checkmate::assert_logical(byrow, len = 1)
#     if (!byrow) {
#       argument <- t(argument)
#     }
#     if (how %in% c("similar", "dissimilar")) {
#       checkmate::assert_integerish(ignore)
#     }
#     vector_flag <- FALSE
#   } else {
#     cli::cli_abort(
#       "Subsetting can only be applied to objects of class {.cls vector} (of
#       length greater than one), {.cls matrix}, or {.cls data.frame}.",
#       call = NULL
#     )
#   }
#   n <- nrow(argument)
#   m <- ceiling(n * proportion)
#   if (how == "random") {
#     ind <- sort(sample.int(n, m))
#   } else if (how == "first") {
#     ind <- seq_len(m)
#   } else if (how == "last") {
#     ind <- utils::tail(seq_len(n), m)
#   } else {
#     argument_ign <- argument
#     if (length(ignore) > 0) {
#       argument_ign <- argument_ign[, -ignore, drop = FALSE]
#     }
#     cluster <- stats::kmeans(argument_ign, centers = centers)$cluster
#     ind <- integer(0)
#     if (how == "similar") {
#       i <- 1
#       while (length(ind) < m && i <= centers) {
#         ind_i <- which(cluster == i)
#         ind <- c(ind, ind_i[seq_len(min(m - length(ind), length(ind_i)))])
#         i <- i + 1
#       }
#     } else if (how == "dissimilar") {
#       ind_cluster <- split(1:n, cluster)
#       i <- 0
#       while (length(ind) < m) {
#         i_mod <- i %% centers + 1
#         i <- i + 1
#         if (length(ind_cluster[[i_mod]]) == 0) next
#         ind <- c(ind, ind_cluster[[i_mod]][1])
#         ind_cluster[[i_mod]] <- ind_cluster[[i_mod]][-1]
#       }
#     }
#     ind <- sort(ind)
#   }
#   argument <- argument[ind, , drop = FALSE]
#   if (vector_flag) {
#     argument <- argument[, 1]
#   }
#   if (!byrow) {
#     argument <- t(argument)
#   }
#   if (df_flag) {
#     argument <- as.data.frame(argument)
#   }
#   return(argument)
# }
