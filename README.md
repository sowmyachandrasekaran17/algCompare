# algCompareR
The algCompareR package is a statistical benchmarking tool for stringent comparison of the optimization algorithms. The main goal is evaluating a newly proposed algorithm against the benchmarking algorithm. This procedure identifies if the newly proposed algorithm shows practically significant improvement and is robust to uncertainty.

## General Info
 A practically relevant hypothesis testing framework using t-test is implemented that evaluates various key factors like severity, p-value, power. Severity is a meta statistical principle that describes the degree of support to decisions made using classical hypothesis testing. It takes into account the data and performs a post-data evaluation to scrutinize the decisions made by analyzing how well the data fits the hypothesis testing framework. It can be described as the actual power attained in the post data analysis and can be described separately for the decision of either rejecting or not rejecting (accepting) the null hypothesis. The value for severity ranges from 0 to 1. The closer the value to 1, more reliable is the decision made with the hypothesis testing. More importantly, a new measure called Normalized area under the severity discrepancy curve is implemented, which summarizes the support of severity over the discrepancy region of interest. The value of the normalized area under the severity discrepancy curve is closer to 1 then it conveys stronger support for the decision made by the hypothesis testing over the discrepancy region of interest. A value closer to 0 signifies no support. Two implementations of the framework is available, based on the nature of the distribution of the data-sets. A t-test based framework for normal data-sets and a framework that incorporates bootstrapping for non-normal data-sets.

## Installation
The package can be installed directly from R 
```R
install.packages("algCompare")
```


## Prerequisite
The results of the two optimization methods to be compared is needed. 

## Usage
The main function of the algcompareR package which compares the performances of algorithms in presence of user defined uncertainities is:

```R
alg_compare(Alg_bm, Alg_new, alpha = 0.05, discrepancy_range = .001,
  bootstrapping = FALSE,measure="mean")
```
Where an Alg_bm and Alg_new are n-dimensional vector of optimal values of the objective obtained for n runs using the benchmark algorithm  and newly proposed algorithm respectively. Both should be of same size. This function compares Alg_bm and Alg_new and checks if Alg_new outperforms Alg_bm.

alpha -	is the significance level (0< alpha <1, default is 0.05)

discrepancy_range is the range of practically insignificant improvement (application dependent, a user_defined_value, default is .001)

bootstrapping - indicates if bootstrapping needs to be performed with hypothesis testing, default "FALSE", accepts "TRUE" or "FALSE", choose "FALSE" for normal dataset and "TRUE" incase of non-normal datasets

 measure- indicates which measure to be compared among algorithms in the hypothesis testing, can take "mean", "median", "mode".
 
 nsample- the number of bootstrap samples, if needed

The difference between the Alg_bm and Alg_new is calculated.

Hypothesis test:  H0: d <=0  vs. Ha: d > 0, for a minimisation problem.

The test validates the performances over a wide spectrum of discrepancies. If the decision is to not-reject H0, then it returns  sev_notRejectH0, which is the severity of how strongly it supports the decision over the range of discrepancies. Or if the decision is to reject H0, then it returns  sev_rejectH0, which is the severity of how strongly it supports the decision of rejecting H0 over the range of discrepancies. 

Aditionally, the severity measure and power is plotted for the chosen discrepancy range in a single plot. The sev_auc consolidates te normlaized area under the severity discrepancy curve.

The function returns the below list of performance metrics

decision - either as not reject or rejected H0 based on the p-value

p - the p-value of the test

sev_notRejectH0 - a numeric vector of severity measure for not rejecting the null hypothesis for the range of discrepancies

sev_rejectH0 - a numeric vector of severity measure for rejecting the null hypothesis for the range of discrepancies

discrepancy - the range of user defined discrepancy

power - a numeric vector comprising the power function corresponding to each discrepancy

sev_auc - Normalized area under the severity discrepancy curve
## Examples
```R
## Prerequisite 1: Create two vectors A and B. Let A and B be the optimum achieved.
A <-c(0.1,0.03,0.05,0.08,0.04)
B <- c(0.01,0.01,0.01,0.05,0.02)

## Example 1: Compare A and B to check if B outperforms A.
## H0: B does not outperform A vs Ha: B outperforms A
result_example1 <- alg_compare(A,B,discrepancy_range=.001,bootstrapping=FALSE)
result_example1.1 <- alg_compare(A,B,discrepancy_range=.02,bootstrapping=FALSE)
result_example1.2 <- alg_compare(A,B,bootstrapping=FALSE)

## Example 2: Compare A and B to check if A outperforms B.
##H0: A does not outperform B vs Ha: A outperforms B
result_example2 <- alg_compare(B,A,bootstrapping=FALSE)

## Prerequisite 2: Create sphere function and optimize it with Nelder-Mead and SANN for 10 runs
# Sphere function
sphere <- function(xx)
{
sum <- sum(xx^2)
y <- sum
return(y)
}
 ## Initialization of variables
nm_optim <- NULL
sann_optim <-NULL
runs <- 10
set.seed(123)
## Optimizing Sphere function with NM and SANN
for(i in 1:runs){
 start <- (runif(2,min=(-5),max=(5)))# random initial start at each run
 nm <-optim(start, sphere, method = "Nelder-Mead")
 nm_mean <- (nm$value)
 nm_optim <- c(nm_optim,nm_mean)# Complete Results for NM Algorithm
 sann <- optim(start, sphere, method = "SANN")
 sann_mean <- (sann$value)
 sann_optim <- c(sann_optim,sann_mean)# Complete Results for SANN Algorithm
}

## Example 3:
## Compare Nelder-Mead and SANN to check if NM outperforms SANN
## Alg_bm <- SANN, Alg_new <- NM, Tests if NM outperforms SANN
## ( Null hypothesis H0: Alg_new does not outperform Alg_bm
## Alt hypothesis Alg_new outperforms Alg_bm)
result_nm_outperforms_sann <- alg_compare(sann_optim,nm_optim,bootstrapping=FALSE)

## Example 4:
## Compare Nelder-Mead and SANN to check if SANN outperforms NM
 result_sann_outperforms_nm <- alg_compare(nm_optim,sann_optim,bootstrapping=TRUE)
```
