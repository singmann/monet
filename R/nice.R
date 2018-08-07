#' Make nice ANOVA table for printing.
#'
#' This generic function produces a nice ANOVA table for objects of class
#'  \code{"monet"} as returned by \code{\link{test_terms}()}.
#'
#' @param object,x An object of class \code{"monet"}.
#' @param sig_symbols Character. What should be the symbols designating
#'    significance? When entering an vector with \code{length(sig.symbol) < 4}
#'    only those elements of the default (\code{c(" +", " *", " **", " ***")})
#'    will be replaced. \code{sig_symbols = ""} will display the stars but not
#'    the \code{+}, \code{sig_symbols = rep("", 4)} will display no symbols.
#'    The default is given by \code{afex_options("sig_symbols")}.
#' @param ... currently ignored.
#'
#' @note Same functionality as the function with the same name from \pkg{afex}.
#'
#' @export nice
nice <- function(object, ...) UseMethod("nice", object)

#' @rdname nice
#' @method nice monet
#' @export
nice.monet <- function(object,
                       sig_symbols = attr(object$anova_table, "sig_symbols"),
                       ...) {
  anova_table <- object$anova_table
  #dots <- list(...)
  if(is.null("sig_symbols")) sig_symbols <- c(" +", " *", " **", " ***")
  symbols.use <-  c(" +", " *", " **", " ***")
  symbols.use[seq_along(sig_symbols)] <- sig_symbols

  stat_col <- colnames(anova_table)[ncol(anova_table)-1]
  p_col <- colnames(anova_table)[ncol(anova_table)]

  df.out <- data.frame(Effect = row.names(anova_table), anova_table,
                       check.names = FALSE)
  df.out[[stat_col]] <- make.stat(anova_table, stat = stat_col,
                                     symbols.use)
  df.out[[p_col]] <- round_ps(df.out[[p_col]])

  rownames(df.out) <- NULL
  attr(df.out, "heading") <- attr(anova_table, "heading")
  attr(df.out, "sig_symbols") <- symbols.use
  class(df.out) <- c("nice_table", class(df.out))
  df.out
}

make.stat <- function(anova, stat, symbols) {
  out <- ifelse(anova[[paste0("Pr(>", stat,")")]] < 0.001,
         paste0(formatC(anova[[stat]], digits = 2, format = "f"), symbols[4]),
         ifelse(anova[[paste0("Pr(>", stat,")")]] < 0.01,
                paste0(formatC(anova[[stat]], digits = 2, format = "f"),
                       symbols[3]),
                ifelse(anova[[paste0("Pr(>", stat,")")]] < 0.05,
                       paste0(formatC(anova[[stat]], digits = 2, format = "f"),
                              symbols[2]),
                       ifelse(anova[[paste0("Pr(>", stat,")")]] < 0.1,
                              paste0(formatC(anova[[stat]], digits = 2,
                                             format = "f"), symbols[1]),
                              formatC(anova[[stat]], digits = 2,
                                      format = "f")))))
  out[is.na(anova[[paste0("Pr(>", stat,")")]])] <- formatC(
    anova[[stat]][is.na(anova[[paste0("Pr(>", stat,")")]])], digits = 2,
    format = "f")
  out
}
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
  abs(x - round(x)) < tol

print_legend <- function(x) {
  sig_symbols <- as.character(attr(x, "sig_symbols"))
  if(length(sig_symbols) > 0 & !all(sig_symbols == rep("", 4))) {
    sleg <- attr(stats::symnum(0, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                               symbols = rev(c(" " , trimws(sig_symbols)))),
                 "legend")
    width <- getOption("width")

    if(width < nchar(sleg)) {
      sleg <- strwrap(sleg, width = width - 2, prefix = "  ")
    }

    cat("---\nSignif. codes:  ", sleg, sep = "", fill = getOption("width") +
          4 + max(nchar(sleg, "bytes") - nchar(sleg)))
  }
}

round_ps <- function(x) {
  substr(as.character(
    ifelse(x < 0.0001, " <.0001",
           ifelse(x < 0.001, formatC(x, digits = 4, format = "f"),
                  ifelse(x < 0.01, formatC(x, digits = 3, format = "f"),
                         ifelse(round(x, 2) == 1, " >.99",
                                formatC(x, digits = 2, format = "f")))))), 2, 7)
}

#' @rdname nice
#' @method print nice_table
#' @export
print.nice_table <- function(x, ...) {
  if(!is.null(heading <- attr(x, "heading"))) {
    cat(heading, sep = "\n")
  }
  print.data.frame(x)
  if(!is.null(attr(x, "sig_symbols"))) print_legend(x)
  if(!is.null(correction_method <- attr(x, "correction")) &&
     correction_method != "none") {
    cat("\nSphericity correction method:", correction_method, "\n")
  }
  invisible(x)
}
