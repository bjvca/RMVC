library(DeclareDesign)
library(dplyr)
library(future)
options(parallelly.fork.enable = TRUE)
plan(multicore, workers = 10)
#treatment = provide a price incentive for quality
#outcome = quality of the product
ICC <- .11
n_traders <- 100
farmers_per_trader <- 5
effect_size_trader <- .5

M <- declare_model(
	traders = add_level(
		N=500,
		U = rnorm(N, mean = 3.939, sd = .18), X = rnorm(N, mean = 0, sd = 117*sqrt(ICC)),
		potential_outcomes(Y_T ~ effect_size_trader*Z + U)),
	farmers = add_level(
		N=50, 
		farmer_error = rnorm(N, mean = 0, sd = 117*sqrt(1-ICC)),
		baseline_price = 1024 + X + farmer_error,
		potential_outcomes(Y_F ~ baseline_price +effect_size_trader*100*.75*Z))  #we assume that half of the effect trckles through

	)+
declare_inquiry(
	ATE_T = mean(Y_T_Z_1 - Y_T_Z_0),
	ATE_F = mean(Y_F_Z_1 - Y_F_Z_0)
	)+

  declare_sampling(
		       S_trader = cluster_rs(clusters =traders, n = n_traders),
		           filter = S_trader == 1) +
  declare_sampling(
		       S_farmer = strata_rs(strata = traders, n = farmers_per_trader),
		           filter = S_farmer == 1) +
declare_assignment(Z=cluster_ra(clusters = traders))+
declare_measurement(Y_T = reveal_outcomes(Y_T ~ Z),
	Y_F = reveal_outcomes(Y_F ~ Z))+
declare_estimator(Y_T ~ Z,.method = lm_robust,clusters = traders,  inquiry = "ATE_T",label = "OLS_T")+
declare_estimator(Y_F ~ Z,.method = lm_robust,clusters = traders, inquiry = "ATE_F",label = "OLS_F")
#how much milk would a trader sell to an MCC every day? About 250 liters?
#treatment is assigned at the trader level so we only pay price premium to them
#they get paid the effect size times 10UG. We run the project for 30 days and this is paid to only treatment traders
### we add survey cost of 25 usd per survey and 10000 usd for the consultants
diagnosands <-  declare_diagnosands(power = mean(p.value <= 0.05), cost = mean(effect_size_trader*100*30*250/3688 * (n_traders)/2 + n_traders*farmers_per_trader*25+n_traders*25 +10000))

diagn <- diagnose_design(M, sims = 1000, diagnosands = diagnosands)
print(diagn)
	 
