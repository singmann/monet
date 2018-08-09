#' Testing Model Terms via Nested Models
#'
#' Generic interfaces that allows Type III tests of model terms (i.e., main
#'   effects and interactions) via nested model comparisons using a
#'   user-supplied estimation/fitting function and user-supplied function for
#'   comparing two models. The default function for testing model terms is
#'   \code{\link{anova}} which returns likelihood-ratio tests.
#' @inheritParams nested_model_formulas
#' @param est_fun Estimation/fitting function. For example, \code{lm}, \code{lmer}, ...
#' @param arg_est \code{list} of additional argments passed to \code{arg_est}.
#' @param test_fun Function comparing two models. Needs to return a
#'   \code{data.frame} with one row and the last two columns
#'   need to be the test statistics (e.g., \code{F}, \code{Chisq}) and the
#'   corresponding p-value (e.g., \code{Pr(>F)}, \code{Pr(>Chisq)}). Default is
#'   \code{\link{anova_df}} which is a wrapper for the generic \code{anova}
#'   function that autodetects relevant columns.
#' @param arg_test additional argument passed to \code{test_fun}. See examples
#'   for how to use it with the default \code{test_fun}.
#'
#' @example examples/examples.test_terms.R
#' @export
test_terms <- function(formula, data, extra_formula,
                       est_fun, arg_est = list(),
                       test_fun = anova_df, arg_test = list(),
                       # rev_test_order = FALSE,
                       type = 3,
                       test_intercept = FALSE,
                       na.action) {

  mc <- match.call()
  if (type == 3) type <- "III"

  prep_formulas <- nested_model_formulas(formula = formula,
                                         data = data,
                                         extra_formula = extra_formula,
                                         type = type,
                                         test_intercept = test_intercept,
                                         na.action = na.action)

  est_fun_tmp <- function(x) {
    do.call(est_fun, args = c(formula = x,
                              data = list(prep_formulas$data),
                              arg_est))
  }
  all_fit <- lapply(prep_formulas$formulas, est_fun_tmp)

  print_message <- TRUE
  test_fun_tmp <- function(x) {
    do.call(test_fun, args = c(object = list(x), list(all_fit[[1]]),
                               arg_test))
  }
  # if (rev_test_order) {
  #   test_fun_tmp <- function(x) {
  #     do.call(test_fun, args = c(object = list(x), list(all_fit[[1]]),
  #                                arg_test))
  #   }
  # } else {
  #   test_fun_tmp <- function(x) {
  #     do.call(test_fun, args = c(object = list(all_fit[[1]]), list(x),
  #                                arg_test))
  #   }
  # }
  anova_table <- do.call("rbind", lapply(all_fit[-1], test_fun_tmp))



  ## prepare output:
  class(anova_table) <- c("anova", "data.frame")
  attr(anova_table, "heading") <- c(
    paste0(deparse(mc[["est_fun"]]),
           " Anova Table (Type ", type , " tests)\n"),
    paste0("Model: ", deparse(prep_formulas$formulas[[1]])),
    paste0("Data: " , deparse(mc[["data"]]))
    # paste0("Df full model: ", )
    )
  attr(anova_table, "sig_symbols") <- c(" +", " *", " **", " ***")
  list.out <- list(
    anova_table = anova_table,
    full_model = all_fit[[1]],
    restricted_models = all_fit[-1],
    data = prep_formulas$data) #, type = type, method = method[[1]]
  class(list.out) <- "monet"
  attr(list.out, "type") <- type
  return(list.out)
}


#' @method print monet
#' @export
print.monet <- function(x, ...) {
  tmp <- nice.monet(x, ...)
  print(tmp)
  invisible(tmp)
}

#' @method anova monet
#' @export
anova.monet <- function(object,
                        ...,
                        sig_symbols = attr(object$anova_table, "sig_symbols")) {
  anova_table <- object$anova_table
  attr(anova_table, "sig_symbols") <-
    if (!is.null(sig_symbols)) sig_symbols else
      c(" +", " *", " **", " ***")
  anova_table
}

