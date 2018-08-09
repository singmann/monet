

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("emmeans", quietly = TRUE)) {
        register_s3_method("emmeans", "recover_data", "monet")
        register_s3_method("emmeans", "emm_basis", "monet")
    }
}

### Dynamic registration of S3 methods
# Code borrowed from hms pkg. I omitted some type checks etc. because
# this is only for internal use and I solemnly promise to behave myself.

register_s3_method <- function(pkg, generic, class) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
    if (isNamespaceLoaded(pkg)) {
        registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
    # Register hook in case package is later unloaded & reloaded
    setHook(
        packageEvent(pkg, "onLoad"),
        function(...) {
            registerS3method(generic, class, fun, envir = asNamespace(pkg))
        }
    )
}

