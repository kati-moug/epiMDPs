# Based on EpiModel param.icm code.

# this function allows us to input parameters
# options include named parameters and "..." user-defined parameters

# goal of HR model is 2-group homogeneous mixing with option to track 
# non-health features

# correspondingly, we expect inf.prob(.g2), rec.rate(.g2), act.rate
# if we track non-health features, we expect s.r.percent(.g2)

# the function does a few checks and returns a list of parameters
param.icm.hr <- function(inf.prob, inter.eff, inter.start, act.rate, rec.rate,
                      a.rate, ds.rate, di.rate, dr.rate, inf.prob.g2,
                      act.rate.g2, rec.rate.g2, a.rate.g2, ds.rate.g2,
                      di.rate.g2, dr.rate.g2, balance, 
                      s.r.percent, s.r.percent.g2, save.folder = "",
                      save.csv = FALSE, ...) {
  
  # Get arguments
  p <- list()
  formal.args <- formals(sys.function())
  formal.args[["..."]] <- NULL
  for (arg in names(formal.args)) {
    if (as.logical(mget(arg) != "")) {
      p[arg] <- list(get(arg))
    }
  }
  dot.args <- list(...)
  names.dot.args <- names(dot.args)
  if (length(dot.args) > 0) {
    for (i in seq_along(dot.args)) {
      p[[names.dot.args[i]]] <- dot.args[[i]]
    }
  }
  
  if ("b.rate" %in% names.dot.args) {
    p$a.rate <- dot.args$b.rate
    message("EpiModel 1.7.0 onward renamed the birth rate parameter b.rate to
            a.rate. ", "See documentation for details.")
  }
  if ("b.rate.g2" %in% names.dot.args) {
    p$a.rate.g2 <- dot.args$b.rate.g2
    message("EpiModel 1.7.0 onward renamed the birth rate parameter b.rate.g2 to
            a.rate.g2. ", "See documentation for details.")
  }
  
  ## Defaults and checks
  p$vital <- ifelse(!is.null(p$a.rate) | !is.null(p$ds.rate) |
                      !is.null(p$di.rate) | !is.null(p$dr.rate), TRUE, FALSE)
  
  p$groups <- ifelse(any(grepl(".g2", names(p))) == TRUE, 2, 1)
  
  if (is.null(p$act.rate)) {
    stop("Stochastic ICM HR requires act rate.")
  }
  
  if (p$vital == TRUE) {
    stop("Stochastic ICM HR assumes no arrivals or departures.")
  }
  
  if (p$groups == 1) {
    stop("Stochastic ICM HR model assumes two health risk levels.")
  }
  
  if (!is.null(p$balance) | !is.null(p$act.rate.g2)) {
    stop("Stochastic ICM HR model assumes homogeneous mixing.")
  }
  
  if (!is.null(p$s.r.percent) & is.null(p$s.r.percent.g2)) {
    stop("If s.r.percent is specified, s.r.percent.g2 must be specified.")
  }
  
  if (is.null(p$s.r.percent) & !is.null(p$s.r.percent.g2)) {
    stop("If s.r.percent.g2 is specified, s.r.percent must be specified.")
  }
  
  # check if we should track non-health features of susceptible individuals
  p$features <- any(grepl("s.r.percent", names(p)))
  
  # check that the length of features is 3
  if (p$features) {
    if (length(p$s.r.percent)!=3 | length(p$s.r.percent.g2)!=3) {
      stop("The number of non-health features must be 3.")
    }
  }
  
  
  if (!is.null(p$inter.eff) && is.null(p$inter.start)) {
    p$inter.start <- 1
  }
  
  ## Output
  class(p) <- c("param.icm", "list")
  return(p)
}