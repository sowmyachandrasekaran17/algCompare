### Package algCompare
#' @title Comparison of the performances of the optimization algorithms with user-defined discrepancies
#' @description The alg_compare function performs an upper tail hypothesis testing to identify if \code{Alg_new} outperforms \code{Alg_bm}. The important feature of this function is the severity measure, a key statistic that stringently validates the performance in presence of small discrepancies. This discrepancy accounts for the practically insignificant improvement, which could have occurred due to several factors such as computer accuracy (i.e., floating points), variable types (4-byte float, 8-byte float, 10-byte float), or even the stopping criteria that is the error threshold when the algorithms are stopped.Secondly, discrepancy can be considered as the improvement that is practically insignificant, which is user-defined and depends on the application being tested.
#'
#' The severity support for the decision of not-reject or reject is obtained for the chosen discrepancy range. The normalized area under the severity discrepancy curve is proposed as a new measure, which summarizes the support of severity over the discrepancy region of interest.The value of the normalized area under the severity discrepancy curve is closer to 1 then it conveys stronger support for the decision made by the hypothesis testing over the discrepancy region of interest.A value closer to 0 signifies no support.
#'
#' Additionally, the severity measure and power is plotted for the chosen discrepancy range in a single plot.
#' @details The statement \code{Alg_new} outperforms \code{Alg_bm} is equivalent to \code{Alg_bm} > \code{Alg_new}, which
#'  can be formulated as the statistical hypothesis \code{H0: Alg_bm-Alg_new <= 0}. This hypothesis
#' \code{H0} will be tested against the alternate hypothesis \code{Ha: Alg_bm-Alg_new > 0}, which states that
#'  \code{Alg_new} is better than \code{Alg_bm} for a minimization problem.
#' \itemize{
#' \item Null hypothesis \code{H0}: \code{Alg_new} does not outperform \code{Alg_bm}
#' \item Alternate hypothesis \code{Ha}: \code{Alg_new} outperforms \code{Alg_bm}
#' }
#' @author Sowmya Chandrasekaran
#' @param Alg_bm an \code{n}-dimensional vector of the achieved objective values obtained for \code{n} runs by the benchmark algorithm
#' @param Alg_new an \code{n}-dimensional vector of the achieved objective values obtained for \code{n} runs by the new algorithm
#' @param alpha is the significance level (\code{0< alpha <1}, default is 0.05)
#' @param discrepancy_range is the range of practically insignificant improvement (application dependent, a user_defined_value, default is .001)
#' @param bootstrapping indicates if bootstrapping needs to be performed with hypothesis testing, default "FALSE",
#' accepts "TRUE" or "FALSE", choose "FALSE" for normal data-set and "TRUE" in-case of non-normal data-set
#' @param measure indicates which measure to be compared among algorithms in the hypothesis testing, can take "mean", "median", "mode".
#' @param nsample the number of bootstrap samples, if needed
#' @import stats
#' @import graphics
#' @import ggplot2
#' @importFrom DescTools AUC
#' @export
#' @return returns a list of performance metrics
#' \itemize{
#' \item decision - \code{H0} either as not-reject or rejected based on the p-value
#' \item p - the p-value of the test
#' \item sev_notRejectH0 - a numeric vector of severity measure for not-rejecting the null hypothesis for the range of discrepancies
#' \item sev_rejectH0 - a numeric vector of severity measure for rejecting the null hypothesis for the range of discrepancies
#' \item discrepancy - the range of user defined discrepancy
#' \item power - a numeric vector comprising the power function corresponding to each discrepancy
#' \item sev_auc - Normalized area under the severity discrepancy curve
#' }
#' @examples
#' ## Prerequisite 1: Create two vectors A and B. Let A and B be the optimum achieved.
#' set.seed(123)
#'A <-abs(rnorm(20,mean=.1,sd=1))
#' set.seed(123)
#'B <- abs(rnorm(20,mean=.01,sd=.1))
#'
#' ## Example 1: Compare A and B to check if B outperforms A.
#'## H0: B does not outperform A vs Ha: B outperforms A
#' result_example1 <- alg_compare(A,B,discrepancy_range=.001,bootstrapping=FALSE,measure="mean")
#'
#' ## Interpretation of the result: The decision based on the p-value is to reject H0.
#' ## The severity value always remains close to 1, suggesting that it highly
#' ## supports the decision over the complete range of discrepancy. The value of sev_auc is close to 1 signifying strong support for the decision made by the hypothesis test.
#'
#' ## Example 2: Compare A and B to check if A outperforms B.
#'##H0: A does not outperform B vs Ha: A outperforms B
#' result_example2 <- alg_compare(B,A,bootstrapping=FALSE,measure="mean")
#'
#' ## Interpretation of the result: The decision based on the p-value is to not-reject H0.
#' ## The severity value always remains close to 1, suggesting that it highly supports
#' ## the decision over the complete range of discrepancy. The value of sev_auc is close to 1 signifying strong support for the decision made by the hypothesis test.
#'
#' ## Prerequisite 2: Create sphere function and optimize it with Nelder-Mead and SANN for 10 runs
#' ## Sphere function
#' sphere <- function(xx)
#' {
#' sum <- sum(xx^2)
#' y <- sum
#' return(y)
#' }
#'  ## Initialization of variables
#'nm_optim <- NULL
#'sann_optim <-NULL
#'runs <- 10
#'set.seed(123)
#' ## Optimizing Sphere function with NM and SANN
#'for(i in 1:runs){
#'  start <- (runif(2,min=(-5),max=(5)))# random initial start at each run
#'  nm <-optim(start, sphere, method = "Nelder-Mead")
#'  nm_mean <- (nm$value)
#'  nm_optim <- c(nm_optim,nm_mean)# Complete Results for NM Algorithm
#'  sann <- optim(start, sphere, method = "SANN")
#'  sann_mean <- (sann$value)
#'  sann_optim <- c(sann_optim,sann_mean)# Complete Results for SANN Algorithm
#'}
#'
#' ## Example 3:
#' ## Compare Nelder-Mead and SANN to check if NM outperforms SANN
#' ## Alg_bm <- SANN, Alg_new <- NM, Tests if NM outperforms SANN
#'## ( Null hypothesis H0: Alg_new does not outperform Alg_bm
#' ## Alt hypothesis Ha: Alg_new outperforms Alg_bm)
#' result_nm_outperforms_sann <- alg_compare(sann_optim,nm_optim,bootstrapping=FALSE,measure="mean")
#'
#'  ## Interpretation of the result: The decision based on the p-value is to reject H0.
#' ## The severity value is high until a discrepancy value of 10^-4
#' ## after which it decreases to zero. This signifies that NM outperforms
#' ## SANN only with a very small variation that is less than 10^-4. The value of sev_auc is very less signifying no support for the decision made by the hypothesis test.
#'
#' ## Example 4:
#' ## Compare Nelder-Mead and SANN to check if SANN outperforms NM
#'  result_sann_outperforms_nm <- alg_compare(nm_optim,sann_optim,bootstrapping=FALSE,measure="mean")
#'
#'  ## Interpretation of the result: The decision based on the p-value is to not-reject H0.
#'  ##The severity value always remains close to 1, suggesting that it highly supports
#'  ## the decision over the complete range of discrepancy. The value of sev_auc is close to 1 signifying strong support for the decision made by the hypothesis test.


alg_compare <-  function(Alg_bm,Alg_new,alpha=0.05,discrepancy_range=.001,bootstrapping=FALSE,measure="mean",nsample=10000)
{

  ## Input Validation
  alpha <-  as.numeric(alpha)

    discrepancy_max <- discrepancy_range

    discrepancy_min <- 0
    if(class(discrepancy_max) != "numeric")
    {
      stop("discrepancy_range should be numeric")
    }
  if(class(Alg_bm) != "numeric")
  {
    stop("Input should be numeric vector")
  }
  if(class(Alg_new) != "numeric")
  {
    stop("Input should be numeric vector")
  }

  if(length(Alg_bm)!=length(Alg_new)){
    stop("Both Alg_bm and Alg_new should be of the same size in order to compare!")
  }
  if(!isFALSE(bootstrapping) && !isTRUE(bootstrapping)){
    stop("bootstrapping parameter not correctly chosen. It must be either TRUE or FALSE")
  }
    if(measure!="mean" &&  measure!="median" && measure!="mode" ){
      stop("Please choose appropriate measure. It must be mean, median or mode")
    }
  ##### Handling negative values if observed for Alg_bm, Alg_new with bias factor
  bias <- FALSE
  min_Algbm <- 0
  min_Algnew <- 0

  if(any(Alg_bm<0)){
    bias <- TRUE
    min_Algbm <- min(Alg_bm)
  }

  if(any(Alg_new<0)){
    bias <- TRUE
    min_Algnew <- min(Alg_new)
  }

  if(bias==TRUE){

    bias_value <- min(min_Algbm,min_Algnew)
    Alg_bm <- Alg_bm+abs(bias_value)
    Alg_new <- Alg_new+abs(bias_value)
    print("Alg_new")
    print(Alg_new)
    print("Alg_bm")
    print(Alg_bm)
    warning("Negative values were observed for Alg_bm, Alg_new or both. Hence the data is appropriately biased for testing. However this does not affect the comparison.")
  }

  mode_function <- function(m) {
    uniqm <- unique(m)
    uniqm[which.max(tabulate(match(m, uniqm)))]
  }

  ### Hypothesis Testing  Formulation for Normal Data sets###
  if(isFALSE(bootstrapping)){
    d <- (Alg_bm-Alg_new) # difference between both algorithms
    n <- length(d) # total number of data points
if(measure=="mean"){
    d_measure <- mean(d) # mean value of the differences
}
    else if (measure=="median")
    {
      d_measure <- median(d) # median value of the differences
    }
    else if(measure=="mode") # mode value of the differences
    {
      d_measure=mode_function(d)
    }
  #  print(d_measure)
    d_sigma <- sd(d)/ sqrt(n) # standard deviation of the differences
    discrepancy<-  seq(from = discrepancy_min, to = discrepancy_max, length.out = 11) ### setting the level of expected discrepancy

    l <-  length(discrepancy) # length of discrepancy vector
    # Hypothesis formulation
    mu0<- 0 # null hypothesis is that the difference d between the two population means should be zero
    mu1 <-  rep(x = mu0, times = l) + discrepancy # mu1 is mu0 added with discrepancy
    t_alpha <-  qt((1-alpha), df=n-1) # cut-off region for t-distribution, (1-alpha)th quantile of the Student t distribution.
    sigma_x_inv <-  1 / d_sigma
    t <-  sigma_x_inv * (d_measure - mu0) # test statistic, t
    delta1 <-  sigma_x_inv * discrepancy # non-centrality parameter
    p <-  pt(t, df=n-1, lower.tail=FALSE) # p-value, area to the right of the t
  #  print(p)
    power <-  pt(q = rep(x = t_alpha, times = l) - delta1, lower.tail = FALSE, df=n-1) # power
    q1 <-  matrix(nrow = l, ncol = 1)
    q2 <-  matrix(nrow = l, ncol = 1)
    sev_rejectH0 <-  NULL # Severity to reject the null hypothesis
    sev_notRejectH0 <-  NULL  # Severity to not-reject the null hypothesis
    sev_auc <- NULL

    if(t <= t_alpha) # notReject H0 when the observed test statistic is less than the cut off value
    {
      decision <-  "notReject H0" # not-reject null hypothesis
      q2[, 1] <-  sigma_x_inv * (rep(x = d_measure, times = l) - mu1)
      sev_notRejectH0 <-  pt(q2[, 1],df=n-1,lower.tail = FALSE) # severity, area to the right of q2
      sev_auc <- AUC(discrepancy,sev_notRejectH0,method="linear")
      sev_auc <- sev_auc/discrepancy_max
      plot(sev_notRejectH0 ~discrepancy, type = "b",col="green",main="Severity for Not-Rejecting Null Hypothesis",ylim=c(0,1),xaxt="none",pch=19,cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,font.lab=1,ylab="sev_notRejectH0",xlab = "Discrepancy")
      axis(1, seq(discrepancy_min,discrepancy_max,length.out =11))
      lines(x=discrepancy,y=power, col="blue",lty=4,pch=18,type="b")
      par(xpd=NA)
      legend("right",legend = c("Severity", "Power"), lty = c(1,4),col=c("green", "blue"))
      return(list(decision = decision, p = p, "sev_notRejectH0" = sev_notRejectH0, "discrepancy" = discrepancy,"power"=power,"sev_auc"=sev_auc))

    }
    if(t > t_alpha) # Reject H0 when the observed test statistic is greater than the cut off value
    {
      decision <-  "Reject H0" # reject null hypothesis
      q1[, 1] <-  sigma_x_inv * (rep(x = d_measure, times = l) - mu1)
      sev_rejectH0 <-  pt(q1[, 1],df=n-1) # severity, area to the left of q1
      sev_auc <- AUC(discrepancy,sev_rejectH0,method="linear")
      sev_auc <- sev_auc/discrepancy_max
      plot(sev_rejectH0 ~discrepancy, type = "b",col="red",main="Severity for Rejecting Null Hypothesis",ylim=c(0,1),xaxt="none",pch=19,cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,font.lab=1,ylab="Severity_RejectH0",xlab = "Discrepancy")
      axis(1, seq(discrepancy_min,discrepancy_max,length.out =11))
      lines(x=discrepancy,y=power, col="blue",lty=4,pch=18,type="b")
      par(xpd=NA)
      legend("right", legend = c("Severity", "Power"), lty = c(1,4),col=c("red", "blue"))
      return(list(decision = decision, p = p, "severity_rejectH0" = sev_rejectH0, "discrepancy" = discrepancy,"power"=power,"sev_auc"=sev_auc))

    }}



  ######### Non-normal Data-sets bootstrapping with Hypothesis testing
  if(isTRUE(bootstrapping)){
    mu0<- 0 # null hypothesis is that the difference d between the two population means should be zero
    discrepancy<-  seq(from = discrepancy_min, to = discrepancy_max, length.out = 11) ### setting the level of expected discrepancy or uncertainty

    l <-  length(discrepancy) # length of discrepancy vector
    mu1 <-  rep(x = mu0, times = l) + discrepancy # mu1 is mu0 added with discrepancy
    test_statistic <-(mean(Alg_bm-Alg_new))
bootstrapping_data <- c(Alg_bm,Alg_new)
    # Perform bootstrapping with replacement
    n_Alg_bm<-length(Alg_bm)
    n <- length(bootstrapping_data)  # the number of observations to sample

      B <- nsample  # the number of bootstrap samples.


    Boot_test_stat <- rep(0,B)
    Boot_test_stat_sev_a <- matrix(nrow=l,ncol=B) # For severity calculation
    Boot_test_stat_sev_r <- matrix(nrow=l,ncol=B) # For severity calculation
    # Form each Boot-sample as a column
    set.seed(123)   # seed for reproducibility
    Bootstrapped_data <- matrix( sample(bootstrapping_data, size= B*n,
                                  replace=TRUE), ncol=B, nrow=n)

    for (i in 1:B){
      # calculate the bootstrapping-test-statistic
      Boot_test_stat[i] <-(mean(Bootstrapped_data[(1:n_Alg_bm),i])-mean(Bootstrapped_data[(n_Alg_bm+1):nrow(Bootstrapped_data),i]))
    }

    # Calculating critical value
mean_Boot_test_stat <- mean(Boot_test_stat)
sd_Boot_test_stat <-  sd(Boot_test_stat)/ sqrt(length(Boot_test_stat))
sd_Boot_test_stat_inv <-  1 / sd_Boot_test_stat

t_bsmeasure <-(Boot_test_stat-mean_Boot_test_stat)*sd_Boot_test_stat_inv
c_bsalpha <-quantile(t_bsmeasure,prob = (1-alpha)) # Critical value for bootstrapped data

    #calculate the p-value
    p <- mean( Boot_test_stat >= test_statistic)

    for (ii in 1:l){
      for (j in 1:B){
        # calculate the bootstrapping-test-statistic in presence of discrepancies
        Boot_test_stat_sev_r[ii,j] <-abs(mean(Bootstrapped_data[(1:n_Alg_bm),j])-mean(Bootstrapped_data[((n_Alg_bm+1):nrow(Bootstrapped_data)),j])-mu1[ii])

       Boot_test_stat_sev_a[ii,j] <-(mean(Bootstrapped_data[(1:n_Alg_bm),j])-mean(Bootstrapped_data[((n_Alg_bm+1):nrow(Bootstrapped_data)),j])+mu1[ii])

      }}

    sev_rejectH0 <-  NULL
    sev_notRejectH0 <-  NULL
    sev_auc <- NULL
    power <- NULL
# Calculating power
    for (pr in 1:l)
    {
      power <-c(power,mean(Boot_test_stat_sev_a[pr,]*sd_Boot_test_stat_inv >c_bsalpha))
    }

    if(p<=alpha){
      decision <-  "Reject H0" # reject null hypothesis
      #    q1 <- (rep(x=mean(Boot_test_stat),times = l) - mu1)
      for (k in 1:l)
      {
        sev_rejectH0 <- c(sev_rejectH0,mean(Boot_test_stat_sev_r[k,]<=abs(test_statistic))) # severity

      }
      sev_auc <- AUC(discrepancy,sev_rejectH0,method="linear")
      sev_auc <- sev_auc/discrepancy_max

      plot(sev_rejectH0 ~discrepancy, type = "b",col="red",main="Severity for Rejecting Null Hypothesis",ylim=c(0,1),xaxt="none",pch=19,cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,font.lab=1,ylab="Severity_RejectH0",xlab = "Discrepancy")
      axis(1, seq(discrepancy_min,discrepancy_max,length.out =11))
      lines(x=discrepancy,y=power, col="blue",lty=4,pch=18,type="b")
      par(xpd=NA)
      legend("right",legend = c("Severity", "Power"), lty = c(1,4),col=c("green", "blue"))
      return(list(decision = decision, p = p, "severity_rejectH0" = sev_rejectH0, "discrepancy" = discrepancy,"power"=power,"sev_auc"=sev_auc))
    }
    if(p>alpha){
      decision <-  "notReject H0" # not reject null hypothesis
      #    q2 <- (rep(x=mean(Boot_test_stat),times = l) - mu1)
      for (kk in 1:l)
      {
        sev_notRejectH0 <- c(sev_notRejectH0,mean(Boot_test_stat_sev_a[kk,]>(test_statistic))) # severity

      }
      sev_auc <- AUC(discrepancy,sev_notRejectH0,method="linear")
      sev_auc <- sev_auc/discrepancy_max
    #  sev_notRejectH0=1-sev_notRejectH0
      plot(sev_notRejectH0 ~discrepancy, type = "b",col="green",main="Severity for Not-Rejecting Null Hypothesis",ylim=c(0,1),xaxt="none",pch=19,cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,font.lab=1,ylab="sev_notRejectH0",xlab = "Discrepancy")
      axis(1, seq(discrepancy_min,discrepancy_max,length.out =11))
      lines(x=discrepancy,y=power, col="blue",lty=4,pch=18,type="b")
      par(xpd=NA)
      legend("right",legend = c("Severity", "Power"), lty = c(1,4),col=c("green", "blue"))
      return(list(decision = decision, p = p, "sev_notRejectH0" = sev_notRejectH0, "discrepancy" = discrepancy,"power"=power,"sev_auc"=sev_auc))
    }
  }



}
