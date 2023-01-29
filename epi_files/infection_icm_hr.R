# This code adapted from EpiModel infection.icm and infection.icm.bip
infection.icm.hr <- function(dat, at) {
  if (dat$param$save.csv) {
    folder <- dat$param$save.folder
    
    if (!dir.exists(folder)) dir.create(folder)
    
    save.str <- paste(folder, "hr time", sep = "")
    write.csv(dat$attr, paste(save.str, at, "step 0 - status.csv"))
  }
  
  ## Expected acts
  population <- dat$epi$num[at - 1] + dat$epi$num.g2[at - 1] 
  acts <- round(dat$param$act.rate * population / 2)

  ## Edgelist
  p1 <- ssample(which(dat$attr$active == 1), acts, replace = TRUE)
  p2 <- ssample(which(dat$attr$active == 1), acts, replace = TRUE)
  
  
  del <- NULL
  if (length(p1) > 0 & length(p2) > 0) {
    del <- data.frame(p1, p2)
    while (any(del$p1 == del$p2)) {
      del$p2 <- ifelse(del$p1 == del$p2,
                       ssample(which(dat$attr$active == 1), 1), del$p2)
    }
    
    if (acts != nrow(del)) stop()
    
    ## Discordant edgelist
    del$p1.stat <- dat$attr$status[del$p1]
    del$p2.stat <- dat$attr$status[del$p2]
    
    del$p1.group <- dat$attr$group[del$p1]
    del$p2.group <- dat$attr$group[del$p2]
    
    del$p1.infn.prob <- ifelse(del$p1.group == 1, dat$param$inf.prob,
                               dat$param$inf.prob.g2)
    del$p2.infn.prob <- ifelse(del$p2.group == 1, dat$param$inf.prob,
                               dat$param$inf.prob.g2)
    
    if (dat$param$save.csv) {
      write.csv(del, paste(save.str, at, "step 1 indices.csv"))
    }
    
    serodis <- (del$p1.stat == "s" & del$p2.stat == "i") |
      (del$p1.stat == "i" & del$p2.stat == "s")
    del <- del[serodis == TRUE, ]
    
    
    serodis.acts <- nrow(del)
    
    ## Transmission on edgelist
    if (nrow(del) > 0) {
      del$tprob <- ifelse(del$p1.stat == "s", del$p1.infn.prob,
                          del$p2.infn.prob)
      
      if (!is.null(dat$param$inter.eff) && at >= dat$param$inter.start) {
        del$tprob <- del$tprob * (1 - dat$param$inter.eff)
      }
      del$trans <- rbinom(nrow(del), 1, del$tprob)
      
      if (dat$param$save.csv) {
        write.csv(del, paste(save.str, at, "step 2 prob trans.csv"))
      }
      
      del <- del[del$trans == TRUE, ]
      if (nrow(del) > 0) {
        newIds <- unique(ifelse(del$p1.stat == "s", del$p1, del$p2)) 
        newIdsg1 <- newIds[which(dat$attr$group[newIds] == 1)]
        newIdsg2 <- newIds[which(dat$attr$group[newIds] == 2)]
        if (length(newIdsg1)+length(newIdsg2)!=length(newIds)) stop()
        nInf <- length(newIdsg1)
        nInfg2 <- length(newIdsg2)
        newIds <- c(newIdsg1, newIdsg2)
        dat$attr$status[newIds] <- "i"
        dat$attr$infTime[newIds] <- at
      } else {
        nInf <- nInfg2 <- 0
      }
    } else {
      nInf <- nInfg2 <- 0
    }
  } else {
    nInf <- nInfg2 <- 0
  }
  
  
  ## Output
  if (at == 2) {
    dat$epi$si.flow <- c(0, nInf)
    dat$epi$si.flow.g2 <- c(0, nInfg2)
    dat$epi$acts <- c(0, acts)
    dat$epi$serodis.acts <- c(0, serodis.acts)
    dat$epi$new.infns <- c(0, nInf + nInfg2)
  } else {
    dat$epi$si.flow[at] <- nInf
    dat$epi$si.flow.g2[at] <- nInfg2
    dat$epi$acts[at] <- acts
    dat$epi$serodis.acts[at] <- serodis.acts
    dat$epi$new.infns[at] <- nInf + nInfg2
  }
  
  return(dat)
  
}