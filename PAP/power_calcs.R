#run in /home/bjvca/data/projects/OneCG/RMVC/PAP/
path <- getwd()
dta <- read.csv(paste(path,"data/farmers.csv", sep = "/"))


#run in /home/bjvca/data/projects/OneCG/MIPP/study_design/
possible.ns <- seq(from=100, to=1000, by=10)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N

dta$price <- as.numeric(as.character(dta$dairy.sales.q96))
dta$dairy.sales.R4 <- as.numeric(as.character(dta$dairy.sales.R4))
dta$price[!is.na(dta$dairy.sales.R4)] <- dta$dairy.sales.R4[!is.na(dta$dairy.sales.R4)]

summary(outcome_mcc)
#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <- sample(dta$price, size=possible.ns[j],replace=TRUE)  
    tau <- 40                                       # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05  }
  }
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1),type="l")

dta$dairy.sales.R17 <- as.numeric(as.character(dta$dairy.sales.R17))


rbinom(n=possible.ns[j], size=1, prob=.061) 

#run in /home/bjvca/data/projects/OneCG/MIPP/study_design/
possible.ns <- seq(from=100, to=2000, by=10)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N

summary(outcome_mcc)
#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <- rbinom(n=possible.ns[j], size=1, prob=.061) 
  # Hypothesize treatment effect
    Y1 <- rbinom(n=possible.ns[j], size=1, prob=.1)                               # treatment potential outcome
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05  }
  }
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1),type="l")