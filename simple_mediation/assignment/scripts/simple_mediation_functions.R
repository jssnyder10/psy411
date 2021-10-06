## Home Made Bootstrap Function for Simple Mediation
simpMedBoot <- function(sampVec, sampSize, dat, X, M, Y) {
  ### Overview
  ## This function performs bootstrap calculations for 
  ## simple mediation models.
  
  ### Inputs
	## sampVec: Numeric vector reflecting individuals in a sample
	## sampSize: Total sample size -- set as numeric
	## dat: Name of data frame
	## X: Name of predictor -- set as character
	## M: Name of mediator -- set as character
	## Y: Name of criterion -- set as character
	
  ## Create bootstrap sample from data object
	boot_sample = dat[sample(sampVec, sampSize, replace = TRUE), ]
	
	## Mediator as criterion OLS model
	boot_2 = lm(boot_sample[,M] ~ boot_sample[,X], 
	            data = boot_sample)
	
	## Outcome as criterion OLS model
	boot_3 = lm(boot_sample[,Y] ~ boot_sample[,X] + boot_sample[,M], 
	            data = boot_sample)
	
	## a coefficient from mediator model
	a_boot = coef(boot_2)[2]
	
	## b coefficient from outcome model
	b_boot = coef(boot_3)[3]
	
	## mediation (indirect) effect
	ab_boot = a_boot * b_boot
	
	## direct effect from outcome model
	cp_boot = coef(boot_3)[2]
	
	## total effect
	c_boot = ab_boot + cp_boot 
	
	## Return the calculated values
	return(data.frame(a_boot, b_boot, ab_boot,
	                  cp_boot,c_boot, row.names = NULL))
}


## Home Made Monte Carlo Function for Simple Mediation
simpMedCarlo <- function(a, b, cp, se_a, se_b, se_cp) {
  ### Overview
  ## This function performs Monte Carlo calculations for 
  ## simple mediation models.
  
  ### Inputs
  ## a: predictor to mediator effect
	## b: mediator to criterion effect
	## cp: residue effect of predictor to criterion
	## se_a: standard error of a effect
	## se_b: standard error of b effect
	## se_c: standard error of cp effect
	
  ## sample a coefficient
	a_carlo = rnorm(1, mean = a, sd = se_a)
	
	## sample b coefficient
	b_carlo = rnorm(1, mean = b, sd = se_b)
	
	## mediation (indirect) effect
	ab_carlo = a_carlo*b_carlo
	
	## sample c coefficient
	cp_carlo = rnorm(1, mean = cp, sd = se_cp)
	
	## total effect
	c_carlo = ab_carlo + cp_carlo
	
	## Return the calculated values
	return(data.frame(a_carlo, b_carlo, ab_carlo,
	                  cp_carlo, c_carlo, row.names = NULL))
}
