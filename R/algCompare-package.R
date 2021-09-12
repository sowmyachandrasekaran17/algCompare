#' @title algCompare-Package Description
#' @description
#' The algCompare package is a statistical benchmarking tool for stringent comparison of the optimization algorithms. The main goal is evaluating a newly proposed algorithm against the benchmarking algorithm. This procedure identifies if the newly proposed algorithm shows practically significant improvement and is robust to uncertainty.
#'
#'
#' @details  A practically relevant hypothesis testing framework using t-test is implemented that evaluates various key factors like severity, p-value, power and the normalized area under the severity discrepancy curve. Severity is a meta statistical principle that describes the degree of support to decisions made using classical hypothesis testing. It takes into account the data and performs a post-data evaluation to scrutinize the decisions made by analyzing how well the data fits the hypothesis testing framework. It can be described as the actual power attained in the post data analysis and can be described separately for the decision of either rejecting or not rejecting (accepting) the null hypothesis. The value for severity ranges from 0 to 1. The closer the value to 1, more reliable is the decision made with the hypothesis testing. More importantly, a new measure called Normalized area under the severity discrepancy curve is implemented, which summarizes the support of severity over the discrepancy region of interest. The value of the normalized area under the severity discrepancy curve is closer to 1 then it conveys stronger support for the decision made by the hypothesis testing over the discrepancy region of interest. A value closer to 0 signifies no support. Two implementations of the framework is available, based on the nature of the distribution of the data-sets. A t-test based framework for normal data-sets and a framework that incorporates bootstrapping for non-normal data-sets.
#'
#' The package is easy to use:
#'
#' - To compare the performances of two algorithms run:\cr
#' > \code{alg_compare()} \cr
#' @author Sowmya Chandrasekaran, Thomas Bartz-Beielstein
#' @name algCompare-package
#' @docType package
#' @import stats
#' @import graphics
#' @import ggplot2
#' @importFrom Rdpack reprompt
NULL
