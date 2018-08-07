#' Create nested models formulas and corresponding data.frame
#' @param formula Two-sided formula. Separate formulas (i.e., nested models) will be created for each model term on right side of this formula. For example, for mixed-effects models this should be the response (i.e., dependent) variable plus the fixed-effects.
#' @param data \code{data.frame} passed to \code{model.frame} holding the variables in \code{formula} and \code{extra_formula}.
#' @param extra_formula Optional one-sided formula that will be added to the resulting formulas. No separate formulas will be built based on this formula. For example, for mixed-effects model this should contain the random-effects.
#' @param type Type of sums of squares. Currently only Type III (i.e., \code{3} or \code{"III"}) are supported.
#' @param test_intercept Logical. Should test for intercept be included in output? Default is \code{FALSE}
#' @param na.action how \code{NA}s are treated. Passed to \code{\link{model.frame}}.
#' @return List with three slots:
#' \describe{
#'   \item{\code{formulas}}{List of named formulas. First entry is the \code{full_model}, each following entry contains the formula for the full model after withholding one model term.}
#'   \item{\code{data}}{\code{data.frame} holding all variables used inthe  \code{formulas}}
#'   \item{\code{reduced_formulas}}{Same as formulars, but without adding the \code{extra_formula}.}
#' }
#' @author Henrik Singmann with contributions from \href{http://stackoverflow.com/q/11335923/289572}{Ben Bolker and Joshua Wiley}.
#' @importFrom stats anova terms reformulate as.formula
#' @importFrom stats model.frame model.matrix
#' @export
nested_model_formulas <- function(formula, data,
                                extra_formula,
                                type = 3,
                                test_intercept = FALSE,
                                na.action) {

  if (!(as.character(type) %in% c("3", "III"))) {
    stop("Currently only Type III tests implemented.")
  }

  ## to avoid problems with tibbles:
  if (!missing(data))
    data <- as.data.frame(data)


  #mc <- match.call()
  if (!inherits(formula, "formula")) {
    message("Formula (the first argument) converted to formula.")
    formula <- as.formula(formula)
  }


  dv <- as.character(formula)[[2]]

  if (missing(extra_formula)) {
    full_formula <- formula
  } else {
    full_formula <- reformulate(unique(c(all.vars(formula[[3]]),
                                         all.vars(extra_formula))),
                                response = formula[[2]])
  }


  new_data <- model.frame(full_formula, data = data, na.action = na.action)

  all_terms <- attr(terms(formula), "term.labels")
  effect_order <- attr(terms(formula), "order")
  max_effect_order <- max(effect_order)

  m_matrix <- model.matrix(formula, data = new_data)

  fixed_effects <- attr(terms(formula, data = data), "term.labels")
  mapping <- attr(m_matrix, "assign")
  fixed_vars <- all.vars(formula)[-1]

  out_data <- data.frame(new_data, m_matrix)
  tmp_colnames <- colnames(out_data)[-seq_len(ncol(new_data))]

  if (attr(terms(formula, data = data), "intercept") == 1) {
    fixed_effects <- c("(Intercept)", fixed_effects)
  }

  formulas <- vector("list", length(fixed_effects) + 1)
  formulas[[1]] <- formula
  for (i in seq_along(fixed_effects)) {
    formulas[[i+1]] <- reformulate(tmp_colnames[!(mapping == (i-1))],
                response = dv, intercept = FALSE)
  }
  names(formulas) <- c("full_model", fixed_effects)
  if (!test_intercept && fixed_effects[1] == "(Intercept)") {
    fixed_effects <- fixed_effects[-1]
    formulas[["(Intercept)"]] <- NULL
  }

  if (missing(extra_formula)) {
    full_formulas <- formulas
  } else {
    full_formulas <- lapply(formulas, function(x)
      as.formula(paste(deparse(x, width.cutoff = 500L), "+",
                       deparse(extra_formula[[2]], width.cutoff = 500L))))
  }

  out <- list(
    formulas = full_formulas,
    data = out_data,
    reduced_formulas = formulas
  )

  return(out)

}

