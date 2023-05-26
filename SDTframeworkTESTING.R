## here we will setup the logic of a Shiny app or demonstration

## so we can manipulate the matrix values or counts by subject.  Let's do that first.

baseRate <- .1
Ntrials <- 1000
Ntsos <- 3

library(psycho)

# Scenario 1:  High hit rate (90%) relative to miss rate (10%)
#              FA rate relatively high (30%) - borders on reliability dipping below 70%
HR.1 <- baseRate*Ntrials*.9
MR.1 <- baseRate*Ntrials*.1
FA.1 <- (1-baseRate)*Ntrials*.3
CR.1 <- Ntrials - HR.1-MR.1-FA.1

psycho::dprime(HR.1,FA.1,MR.1,CR.1,baseRate*Ntrials,Ntrials-(baseRate*Ntrials),T)


# Scenario 2:  VERY High hit rate (98%) relative to miss rate (2%)
#              FA rate relatively high (30%) - borders on reliability dipping below 70%
HR.1 <- baseRate*Ntrials*.98
MR.1 <- baseRate*Ntrials*.02
FA.1 <- (1-baseRate)*Ntrials*.3
CR.1 <- Ntrials - HR.1-MR.1-FA.1

psycho::dprime(HR.1,FA.1,MR.1,CR.1,baseRate*Ntrials,Ntrials-(baseRate*Ntrials),T)

# Scenario 3:  High hit rate (90%) relative to miss rate (10%)
#              FA rate relatively high (30%) - borders on reliability dipping below 70%
HR.1 <- baseRate*Ntrials*.98
MR.1 <- baseRate*Ntrials*.02
FA.1 <- (1-baseRate)*Ntrials*.1
CR.1 <- Ntrials - HR.1-MR.1-FA.1

psycho::dprime(HR.1,FA.1,MR.1,CR.1,baseRate*Ntrials,Ntrials-(baseRate*Ntrials),T)


##TTD:
## 1. Get 2x2 matrix into a shiny app
## 2. Create structure for shiny app
## 3. Brainstorm graphics for the shiny app
## 4. Investigate input options
## 5. Different bag scenarios in a tab
## 6. Different TSO scenarios across different bags
  
