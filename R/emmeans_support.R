
recover_data.monet <- function(object, ...) {
  emmeans::recover_data(object$full_model, ...)
}

emm_basis.monet <- function(object, trms, xlev, grid, ...) {
  emmeans::emm_basis(object$full_model, trms, xlev, grid, ...)
}

