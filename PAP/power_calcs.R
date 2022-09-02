#run in /home/bjvca/data/projects/OneCG/RMVC/PAP/
path <- getwd()

library(plotly)

library(lmtest)
library(car)
library(clubSandwich)


#run in /home/bjvca/data/projects/OneCG/MIPP/study_design/
N_MCCs <- seq(from=90, to=125, by=5)     # The sample sizes we'll be considering
n_farmers_per_MCC <- seq(from=10, to=40, by=5)
power <- matrix(NA, length(N_MCCs),length(n_farmers_per_MCC))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N

#### Outer loop to vary the number of subjects ####
for (j in 1:length(N_MCCs)){
  for (k in 1:length(n_farmers_per_MCC)){
  N <- N_MCCs[j]                              # Pick the jth value for N
  N1 <- n_farmers_per_MCC[k]
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <- rnorm(N, mean=8, sd=0.4)
    tau <- .25                                       # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Z.sim <- sample(rep(c(0,1),times=N/2))          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim[1:length(Y.sim)])                   # Do analysis (Simple regression)
   Y0_i <- NA
   Z.sim_i <- NA
   clust <- NA
     for (c in 1:length(Y0)) { ###create individual level data
     Y0_i <- c(Y0_i,rnorm(N1, mean=Y0[c], sd=0.6))
     Z.sim_i  <- c(Z.sim_i,rep(Z.sim[c],N1))
     clust <- c(clust,rep(c,N1))
     }
   Y0_i <- Y0_i[2:length(Y0_i)] 
   Z.sim_i <- Z.sim_i[2:length(Z.sim_i)] 
   clust <- clust[2:length(clust)] 
   tau_i <- .2  
    Y1_i <- Y0_i + tau_i
    Y.sim_i <- Y1*Z.sim_i + Y0_i*(1-Z.sim_i)               # Reveal outcomes according to assignment
    fit.sim_i <- lm(Y.sim_i ~ Z.sim_i[1:length(Y.sim_i)])  
     
     tau_i2 <- .12                                  # Hypothesize treatment effect
     Y1_i2 <- Y.sim_i + tau_i2
     Z.sim_i2 <- sample(rep(c(0,1),times=(N*N1)/2))          # Do a random assignment
     Y.sim_i2 <- Y1_i2*Z.sim_i2 + Y.sim_i*(1-Z.sim_i2)
     fit.sim_i2 <- lm(Y.sim_i2 ~ Z.sim_i2[1:length(Y.sim_i2)]+ Z.sim_i[1:length(Y.sim_i)] +Z.sim_i2[1:length(Y.sim_i2)]*Z.sim_i[1:length(Y.sim_i)])  
     vcov_cluster <- vcovCR(fit.sim_i2,cluster=clust,type="CR3")
     coef_test( fit.sim_i2 , vcov_cluster)$p_Satt
  
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    p.value_i <-  coef_test( fit.sim_i2 , vcov_cluster)$p_Satt[3]  # Extract p-values
    p.value_i2 <-  coef_test( fit.sim_i2 , vcov_cluster)$p_Satt[2]  # Extract p-values
    significant.experiments[i] <-  p.value < alpha & p.value_i < alpha & p.value_i2 < alpha 
    
   # print(c(p.value, p.value_i, p.value_i2))
  }
  
    power[j,k] <- mean(significant.experiments)      
      }
   # store average success rate (power) for each N
}


fig <- plot_ly(x=~N_MCCs,y=~n_farmers_per_MCC,z = ~powers)

fig <- fig %>% add_surface(
  
  contours = list(
    
    z = list(
      
      show=TRUE,
      
      usecolormap=TRUE,
      
      highlightcolor="#ff0000",
      
      project=list(z=TRUE)
      
    )
    
  )
  
)

fig <- fig %>% layout(
  
  scene = list(
    
    camera=list(
      
      eye = list(x=1.87, y=0.88, z=-0.64)
      
    )
    
  )
  
)