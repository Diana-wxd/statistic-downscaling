## A function compute all the stats 

skill <- function(pred,obs){
  tab.out <- table(pred, obs)
  
  a <- tryCatch(tab.out[1,1])
  b <- tryCatch(tab.out[1,2])
  c <- tryCatch(tab.out[2,1])
  d <- tryCatch(tab.out[2,2])
  
  n <- a + b + c + d
  s <- (a + c)/n
  TS <- a/(a + b + c)
  POD <- H <- a/(a + c)
  F <- b/(b + d)
  TS.se <- sqrt((TS^2) * ((1 - H)/a + b * (1 - F)/((a + b + 
                                                      c)^2)))
  SH2 <- H * (1 - H)/(a + c)
  SF2 <- F * (1 - F)/(b + d)
  POD.se <- sqrt(SH2)
  F.se <- sqrt(SF2)
  M <- c/(a + c)
  FAR <- b/(a + b)
  FAR.se <- sqrt((FAR^4) * ((1 - H)/a + (1 - F)/b) * (a^2)/(b^2))
  HSS <- 2 * (a * d - b * c)/(1 * (a + c) * (c + d) + 1 * (a + 
                                                             b) * (b + d))
  SHSS2 <- SF2 * (HSS^2) * (1/(H - F) + (1 - s) * (1 - 2 * 
                                                     s))^2 + SH2 * (HSS^2) * (1/(H - F) - s * (1 - 2 * s))^2
  HSS.se = sqrt(SHSS2)
  PSS <- 1 - M - F
  PSS.se <- sqrt(SH2 + SF2)
  KSS <- (a * d - b * c)/((a + c) * (b + d))
  PC <- (a + d)/(a + b + c + d)
  PC.se <- sqrt(s * H * (1 - H)/n + (1 - s) * F * (1 - F)/n)
  BIAS <- (a + b)/(a + c)
  OR <- a * d/(b * c)
  ORSS <- (a * d - b * c)/(a * b + b * c)
  HITSrandom <- 1 * (a + c) * (a + b)/(a + b + c + d)
  p <- (a + c)/n
  ETS <- (a - HITSrandom)/(a + b + c - HITSrandom)
  ETS.se <- sqrt(4 * SHSS2/((2 - HSS)^4))
  theta <- (a * d)/(b * c)
  log.theta <- log(a) + log(d) - log(b) - log(c)
  n.h <- 1/(1/a + 1/b + 1/c + 1/d)
  yules.q <- (theta - 1)/(theta + 1)
  SLOR2 <- 1/n.h
  LOR.se <- sqrt(SLOR2)
  ORSS.se <- sqrt(SLOR2 * 4 * OR^2/((OR + 1)^4))
  eds <- 2 * log((a + c)/n)/log(a/n) - 1
  eds.se <- 2 * abs(log(p))/(H * (log(p) + log(H))^2) * sqrt(H * 
                                                               (1 - H)/(p * n))
  seds <- (log((a + b)/n) + log((a + c)/n))/log(a/n) - 1
  seds.se <- sqrt(H * (1 - H)/(n * p)) * (-log(BIAS * p^2)/(H * 
                                                              log(H * p)^2))
  EDI <- (log(F) - log(H))/(log(F) + log(H))
  EDI.se <- 2 * abs(log(F) + H/(1 - H) * log(H))/(H * (log(F) + 
                                                         log(H))^2) * sqrt(H * (1 - H)/(p * n))
  SEDI <- (log(F) - log(H) - log(1 - F) + log(1 - H))/(log(F) + 
                                                         log(H) + log(1 - F) + log(1 - H))
  SEDI.se <- 2 * abs(((1 - H) * (1 - F) + H * F)/((1 - H) * 
                                                    1 - F) * log(F * (1 - H)) + 2 * H/(1 - H) * log(H * (1 - 
                                                                                                           F)))/(H * (log(F * (1 - H)) + log(H * (1 - F)))^2) * 
    sqrt(H * (1 - H)/(p * n))
  return(list(tab = tab.out, TS = TS, TS.se = TS.se, POD = POD, 
              POD.se = POD.se, M = M, F = F, F.se = F.se, FAR = FAR, 
              FAR.se = FAR.se, HSS = HSS, HSS.se = HSS.se, PSS = PSS, 
              PSS.se = PSS.se, KSS = KSS, PC = PC, PC.se = PC.se, BIAS = BIAS, 
              ETS = ETS, ETS.se = ETS.se, theta = theta, log.theta = log.theta, 
              LOR.se = LOR.se, n.h = n.h, orss = yules.q, orss.se = ORSS.se, 
              eds = eds, eds.se = eds.se, seds = seds, seds.se = seds.se, 
              EDI = EDI, EDI.se = EDI.se, SEDI = SEDI, SEDI.se = SEDI.se))  
}