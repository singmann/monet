#' Wrapper for anova() returning a single data.frame
#' @param ... passed to \code{\link{anova}}
#' @param colnames Determines which columns from \code{as.data.frame(anova(...))}
#'   will be collected. \code{"auto"} attempts to figure out which columns are
#'   relevant via heuristics. Alternatively, a list of character strings. The
#'   first element gives the name(s) of the \code{df} columns,
#'   the second element the column containing the statistic,
#'   the third element, th ecolumn containing the p-value.
#' @return \code{data.frame} with one row and at least three columns.
#' @export
anova_df <- function(...,
                     colnames = "auto") {
  tmp <- as.data.frame(anova(...))
  if (colnames[1] == "auto") {
    coln <- colnames(tmp)
    coln_ndf <- coln[-grep("[dD]f", coln)]
    colnames <- list(
      grep("[dD]f", coln, value = TRUE),
      coln_ndf[length(coln_ndf)-1],
      coln_ndf[length(coln_ndf)]
    )
    coln_df <- paste("Df", rev(seq_along(colnames[[1]]))-1)
    if (dynGet("print_message", FALSE)) {
      message("Detected colnames in anova output:\n ",
            paste0(coln_df, ": ", colnames[[1]], collapse = ", "),
            "\n Statistic: ", colnames[[2]],
            "\n p-value: ", colnames[[3]])
      assign("print_message", FALSE, parent.frame(3))
    }
  } else {
    coln_df <- paste0("Df ", rev(seq_along(colnames[[1]]))-1)
  }

  out <- data.frame(tmp[2,colnames[[1]]],
                    Statistic = tmp[2,colnames[[2]]],
                    "Pr(>Statistic)" = tmp[2,colnames[[3]]],
                    check.names = FALSE)
  colnames(out) <- c(coln_df, colnames[[2]], colnames[[3]])
  out
}
