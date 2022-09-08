#run in /home/bjvca/data/projects/OneCG/RMVC/PAP/
path <- getwd()

library(plotly)

library(lmtest)
library(car)
library(clubSandwich)


#run in /home/bjvca/data/projects/OneCG/MIPP/study_design/
N_MCCs <- seq(from=100, to=135, by=5)     # The sample sizes we'll be considering
n_farmers_per_MCC <- seq(from=10, to=40, by=5)
power <- matrix(NA, length(N_MCCs),length(n_farmers_per_MCC))           # Empty object to collect simulation estimates
power_H1 <- matrix(NA, length(N_MCCs),length(n_farmers_per_MCC))    
power_H2 <- matrix(NA, length(N_MCCs),length(n_farmers_per_MCC))    
power_H3 <- matrix(NA, length(N_MCCs),length(n_farmers_per_MCC))    
power_one <- matrix(NA, length(N_MCCs),length(n_farmers_per_MCC))    
alpha <- 0.05                                    # Standard significance level
sims <- 1000                                     # Number of simulations to conduct for each N

#### Outer loop to vary the number of subjects ####
for (j in 1:length(N_MCCs)){
  for (k in 1:length(n_farmers_per_MCC)){
  N <- N_MCCs[j]                              # Pick the jth value for N
  N1 <- n_farmers_per_MCC[k]
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  significant.experiments_H1 <- rep(NA, sims) 
  significant.experiments_H2 <- rep(NA, sims) 
  significant.experiments_H3 <- rep(NA, sims) 
  significant.experiments_one <- rep(NA, sims) 
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <- rnorm(N, mean=1000, sd=50)
    tau <- 30                                       # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Z.sim <- sample(rep(c(0,1),times=N/2))          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim[1:length(Y.sim)])                   # Do analysis (Simple regression)
   Y0_i <- NA
   Z.sim_i <- NA
   Z.sim_i2 <- NA
   Z.con_i2 <- NA
   clust <- NA
     for (c in 1:length(Y0)) { ###create individual level data
     Y0_i <- c(Y0_i,rnorm(N1, mean=Y0[c]-100, sd=100))
     Z.sim_i  <- c(Z.sim_i,rep(Z.sim[c],N1))
     Z.sim_i2 <-  c(Z.sim_i2,sample(rep(c(0,1),times=(N1/2)))) 
     Z.con_i2 <-  c(Z.con_i2,sample(rep(c(0,1),times=(N1/2)))) 
     clust <- c(clust,rep(c,N1))
     }
   Y0_i <- Y0_i[2:length(Y0_i)] 
   Z.sim_i <- Z.sim_i[2:length(Z.sim_i)] 
   Z.sim_i2 <- Z.sim_i2[2:length(Z.sim_i2)] 
   Z.con_i2 <- Z.con_i2[2:length(Z.con_i2)] 
   clust <- clust[2:length(clust)] 
   tau_i <- 40  
    Y1_i <- Y0_i + tau_i
    Y.sim_i <- Y1_i*Z.sim_i + Y0_i*(1-Z.sim_i)               # Reveal outcomes according to assignment
    fit.sim_i <- lm(Y.sim_i ~ Z.sim_i[1:length(Y.sim_i)])  
     
     tau_i2 <- 25                                  # Hypothesize treatment effect
     Y1_i2 <- Y.sim_i + tau_i2
           # Do a random assignment
     Y.sim_i2 <- Y1_i2*Z.sim_i2 + Y.sim_i*(1-Z.sim_i2)
     fit.sim_i2 <- lm(Y.sim_i2 ~ Z.sim_i[1:length(Y.sim_i2)]*Z.sim_i2[1:length(Y.sim_i2)]*Z.con_i2[1:length(Y.sim_i)]  )  
     vcov_cluster <- vcovCR(fit.sim_i2,cluster=clust,type="CR3")
     coef_test( fit.sim_i2 , vcov_cluster)$p_Satt
  
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    p.value_i <-  coef_test( fit.sim_i2 , vcov_cluster)$p_Satt[2]  # Extract p-values
    p.value_i2 <-  coef_test( fit.sim_i2 , vcov_cluster)$p_Satt[3]  # Extract p-values
    significant.experiments[i] <-  p.value < alpha & p.value_i < alpha & p.value_i2 < alpha 
    significant.experiments_H1[i] <-  p.value < alpha 
    significant.experiments_H2[i] <-  p.value_i < alpha
    significant.experiments_H3[i] <-  p.value_i2 < alpha
    significant.experiments_one[i] <-  p.value < alpha | p.value_i < alpha | p.value_i2 < alpha 
    
   # print(c(p.value, p.value_i, p.value_i2))
  }
  
    power[j,k] <- mean(significant.experiments)
    power_H1[j,k] <- mean(significant.experiments_H1)    
    power_H2[j,k] <- mean(significant.experiments_H2)    
    power_H3[j,k] <- mean(significant.experiments_H3)   
    power_one[j,k] <- mean(significant.experiments_one)  
      }
   # store average success rate (power) for each N
}


fig <- plot_ly(x=~N_MCCs,y=~n_farmers_per_MCC,z = ~power)

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


# fig <- fig %>% add_trace(
#           z = matrix(0.8,dim(powers)[1],dim(powers)[2]),
#           x=~N_MCCs,y=~n_farmers_per_MCC,
#           type = "surface", colorscale = "red", showlegend = F)
v <- ggplot( aes(x=~N_MCCs,y=~n_farmers_per_MCC, z = ~power))
v + geom_contour()