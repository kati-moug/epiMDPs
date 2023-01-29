# Based on EpiModel prevalence code.
prevalence.hr <- function(dat, at) {
  
  if (at == 1) {
    dat$epi <- list()
    if (dat$param$features) {
      dat$epi[["s h0 r0"]] <- sum(dat$attr$active == 1 &
                                  dat$attr$status == "s" &
                                  dat$attr$group == 1 &
                                  dat$attr$feature == 1)
      dat$epi[["s h0 r1"]] <- sum(dat$attr$active == 1 &
                                  dat$attr$status == "s" &
                                  dat$attr$group == 1 &
                                  dat$attr$feature == 2)
      dat$epi[["s h0 r2"]] <- sum(dat$attr$active == 1 &
                                  dat$attr$status == "s" &
                                  dat$attr$group == 1 &
                                  dat$attr$feature == 3)
    }
    dat$epi$s.num <- sum(dat$attr$active == 1 &
                         dat$attr$status == "s" &
                         dat$attr$group == 1)
    
    dat$epi$i.num <- sum(dat$attr$active == 1 &
                           dat$attr$status == "i" &
                           dat$attr$group == 1)
    dat$epi$num <- dat$epi$s.num + dat$epi$i.num
    
    if (dat$param$features) {
      dat$epi[["s h1 r0"]] <- sum(dat$attr$active == 1 &
                                  dat$attr$status == "s" &
                                  dat$attr$group == 2 &
                                  dat$attr$feature == 1)
      dat$epi[["s h1 r1"]] <- sum(dat$attr$active == 1 &
                                  dat$attr$status == "s" &
                                  dat$attr$group == 2 &
                                  dat$attr$feature == 2)
      dat$epi[["s h1 r2"]] <- sum(dat$attr$active == 1 &
                                  dat$attr$status == "s" &
                                  dat$attr$group == 2 &
                                  dat$attr$feature == 3)
    }
    dat$epi$s.num.g2 <- sum(dat$attr$active == 1 &
                              dat$attr$status == "s" &
                              dat$attr$group == 2)
    dat$epi$i.num.g2 <- sum(dat$attr$active == 1 &
                              dat$attr$status == "i" &
                              dat$attr$group == 2)
    dat$epi$num.g2 <- dat$epi$s.num.g2 + dat$epi$i.num.g2
    if (dat$control$type == "SIR") {
      dat$epi$r.num <- sum(dat$attr$active == 1 &
                             dat$attr$status == "r" &
                             dat$attr$group == 1)
      dat$epi$num <- dat$epi$s.num +
        dat$epi$i.num +
        dat$epi$r.num
      dat$epi$r.num.g2 <- sum(dat$attr$active == 1 &
                                dat$attr$status == "r" &
                                dat$attr$group == 2)
      dat$epi$num.g2 <- dat$epi$s.num.g2 +
        dat$epi$i.num.g2 +
        dat$epi$r.num.g2
    }
    dat$epi$s.total <- dat$epi$s.num + dat$epi$s.num.g2
    dat$epi$i.total <- dat$epi$i.num + dat$epi$i.num.g2
    dat$epi$r.total <- dat$epi$r.num + dat$epi$r.num.g2
  } else {
    if (dat$param$features) {
      dat$epi[["s h0 r0"]][at] <- sum(dat$attr$active == 1 &
                                    dat$attr$status == "s" &
                                    dat$attr$group == 1 &
                                    dat$attr$feature == 1)
      dat$epi[["s h0 r1"]][at] <- sum(dat$attr$active == 1 &
                                    dat$attr$status == "s" &
                                    dat$attr$group == 1 &
                                    dat$attr$feature == 2)
      dat$epi[["s h0 r2"]][at] <- sum(dat$attr$active == 1 &
                                    dat$attr$status == "s" &
                                    dat$attr$group == 1 &
                                    dat$attr$feature == 3)
    } 
    dat$epi$s.num[at] <- sum(dat$attr$active == 1 &
                               dat$attr$status == "s" &
                               dat$attr$group == 1)
    dat$epi$i.num[at] <- sum(dat$attr$active == 1 &
                               dat$attr$status == "i" &
                               dat$attr$group == 1)
    dat$epi$num[at] <- dat$epi$s.num[at] + dat$epi$i.num[at]
    if (dat$param$features) {
      dat$epi[["s h1 r0"]][at] <- sum(dat$attr$active == 1 &
                                      dat$attr$status == "s" &
                                      dat$attr$group == 2 &
                                      dat$attr$feature == 1)
      dat$epi[["s h1 r1"]][at] <- sum(dat$attr$active == 1 &
                                      dat$attr$status == "s" &
                                      dat$attr$group == 2 &
                                      dat$attr$feature == 2)
      dat$epi[["s h1 r2"]][at] <- sum(dat$attr$active == 1 &
                                      dat$attr$status == "s" &
                                      dat$attr$group == 2 &
                                      dat$attr$feature == 3)
    } 
    dat$epi$s.num.g2[at] <- sum(dat$attr$active == 1 &
                                  dat$attr$status == "s" &
                                  dat$attr$group == 2)
    dat$epi$i.num.g2[at] <- sum(dat$attr$active == 1 &
                                  dat$attr$status == "i" &
                                  dat$attr$group == 2)
    dat$epi$num.g2[at] <- dat$epi$s.num.g2[at] + dat$epi$i.num.g2[at]
    if (dat$control$type == "SIR") {
      dat$epi$r.num[at] <- sum(dat$attr$active == 1 &
                                 dat$attr$status == "r" &
                                 dat$attr$group == 1)
      dat$epi$num[at] <- dat$epi$s.num[at] +
        dat$epi$i.num[at] +
        dat$epi$r.num[at]
      dat$epi$r.num.g2[at] <- sum(dat$attr$active == 1 &
                                    dat$attr$status == "r" &
                                    dat$attr$group == 2)
      dat$epi$num.g2[at] <- dat$epi$s.num.g2[at] +
        dat$epi$i.num.g2[at] +
        dat$epi$r.num.g2[at]
    }
    dat$epi$s.total[at] <- dat$epi$s.num[at] + dat$epi$s.num.g2[at]
    dat$epi$i.total[at] <- dat$epi$i.num[at] + dat$epi$i.num.g2[at]
    dat$epi$r.total[at] <- dat$epi$r.num[at] + dat$epi$r.num.g2[at]
  }
  
  return(dat)
}