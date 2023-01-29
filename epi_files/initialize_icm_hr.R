# Based on EpiModel initialize.icm code.
print("Warning! All individual categories s.num, i.num, r.num, and .g2
      should be disjoint and cover the entire space. This code assumes their 
      sum to be the number of individuals in the space.")
initialize.icm.hr <- function(param, init, control) {
  
  ## Main List for Data ##
  dat <- list()
  dat$param <- param
  dat$init <- init
  dat$control <- control
  
  
  # Set attributes
  dat$attr <- list()
  numeric.init <- init[which(sapply(init, class) == "numeric")]
  n <- do.call("sum", numeric.init)
  dat$attr$active <- rep(1, n)
  if (dat$param$groups == 1) {
    dat$attr$group <- rep(1, n)
  } else {
    g2inits <- grep(".g2", names(numeric.init))
    g1inits <- setdiff(seq_along(numeric.init), g2inits)
    nG1 <- sum(sapply(g1inits, function(x) init[[x]]))
    nG2 <- sum(sapply(g2inits, function(x) init[[x]]))
    dat$attr$group <- c(rep(1, nG1), rep(2, max(0, nG2)))
  }
  
  # Initialize status and infection time
  dat <- init_status.icm.hr(dat)
  
  # Summary out list
  dat <- do.call(control[["prevalence.FUN"]], list(dat, at = 1))
  
  return(dat)
}


init_status.icm.hr <- function(dat) {
  
  # Variables ---------------------------------------------------------------
  type <- dat$control$type
  group <- dat$attr$group
  nGroups <- dat$param$groups
  
  if (type != "SIR") {
    stop("Stochastic ICM HR model has SIR compartments.")
  }
  
  nG1 <- sum(group == 1)
  nG2 <- sum(group == 2)
  
  i.num <- dat$init$i.num
  r.num <- dat$init$r.num
  i.num.g2 <- dat$init$i.num.g2
  r.num.g2 <- dat$init$r.num.g2
  
  
  # Status ------------------------------------------------------------------
  status <- rep("s", nG1 + nG2)
  status[sample(which(group == 1), size = i.num)] <- "i"
  if ((nGroups == 2) & (i.num.g2 > 0)) {
    status[sample(which(group == 2), size = i.num.g2)] <- "i"
  }
  if (type == "SIR") {
    status[sample(which(group == 1 & status == "s"), size = r.num)] <- "r"
    if ((nGroups == 2) & r.num.g2 > 0) {
      status[sample(which(group == 2 & status == "s"), size = r.num.g2)] <- "r"
    }
  }
  dat$attr$status <- status
  
  if (dat$param$features) {
    s.num <- dat$init$s.num
    s.num.g2 <- dat$init$s.num.g2
    
    s.r.percent <- dat$param$s.r.percent
    s.r.percent.g2 <- dat$param$s.r.percent.g2
    
    nS1.r1 <- round(s.num * s.r.percent[1])
    nS1.r2 <- round(s.num * s.r.percent[2])
    nS1.r3 <- round(s.num * s.r.percent[3])
    
    if (nS1.r1+nS1.r2+nS1.r3 != s.num) {
      stop("Invalid s.r.percent values for initial s.num.")
    }
    
    nS2.r1 <- round(s.num.g2 * s.r.percent.g2[1])
    nS2.r2 <- round(s.num.g2 * s.r.percent.g2[2])
    nS2.r3 <- round(s.num.g2 * s.r.percent.g2[3])
    
    if (nS2.r1+nS2.r2+nS2.r3 != s.num.g2) {
      stop("Invalid s.r.percent.g2 values for initial s.num.g2.")
    }
    
    feature <- rep(0, nG1+nG2)
    
    if (nS1.r1 > 0) feature[which(status == "s" & group == 1)[1:nS1.r1]] <- 1
    if (nS1.r2 > 0) feature[which(status == "s" & group == 1 
                                  & feature == 0)[1:nS1.r2]] <- 2
    if (nS1.r3 > 0) feature[which(status == "s" & group == 1
                                  & feature == 0)[1:nS1.r3]] <- 3
    if (nS2.r1 > 0) feature[which(status == "s" & group == 2
                                  & feature == 0)[1:nS2.r1]] <- 1
    if (nS2.r2 > 0) feature[which(status == "s" & group == 2
                                  & feature == 0)[1:nS2.r2]] <- 2
    if (nS2.r3 > 0) feature[which(status == "s" & group == 2
                                  & feature == 0)[1:nS2.r3]] <- 3
    
    if (length(which(feature == 0)) != (i.num + r.num + i.num.g2 + r.num.g2)) {
      stop(feature)
    }
    
    dat$attr$feature <- feature
  }
  
  # Infection Time ----------------------------------------------------------
  idsInf <- which(status == "i")
  infTime <- rep(NA, length(status))
  
  # If vital=TRUE, infTime is a uniform draw over the duration of infection
  if (dat$param$vital == TRUE && dat$param$di.rate > 0) {
    infTime[idsInf] <- -rgeom(n = length(idsInf), prob = dat$param$di.rate) + 2
  } else {
    if (dat$control$type == "SI" || dat$param$rec.rate == 0) {
      # infTime a uniform draw over the number of sim time steps
      infTime[idsInf] <- ssample(1:(-dat$control$nsteps + 2),
                                 length(idsInf), replace = TRUE)
    } else {
      if ((nGroups == 1) | dat$param$rec.rate == dat$param$rec.rate.g2) {
        infTime[idsInf] <- ssample(1:(-round(1 / dat$param$rec.rate) + 2),
                                   length(idsInf), replace = TRUE)
      }
      if (nGroups == 2) {
        infG1 <- which(status == "i" & group == 1)
        infTime[infG1] <- ssample(1:(-round(1 / dat$param$rec.rate) + 2),
                                  length(infG1), replace = TRUE)
        infG2 <- which(status == "i" & group == 2)
        infTime[infG2] <- ssample(1:(-round(1 / dat$param$rec.rate.g2) + 2),
                                  length(infG2), replace = TRUE)
      }
    }
  }
  dat$attr$infTime <- infTime
  
  return(dat)
}