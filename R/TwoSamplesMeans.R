#' @title Hypothesis tests for two means
#'
#' @description
#' Tests for means of two independent or paired groups.
#' @details
#' \code{TwoSamplesMeans} function performs hypothesis
#' tests on means of independent or paired two
#' groups. Moreover, this function can decide whether it
#' will use a parametric or non-parametric test.
#'
#' @importFrom stats shapiro.test wilcox.test t.test
#' @importFrom BSDA z.test
#' @param x a numeric vector for the data of the first group.
#' @param y a numeric vector for the data of the second group.
#' @param var.equal a logical variable indicating whether to treat
#'        the two variances as being equal
#' @param H1 a character string specifying the alternative
#'        hypothesis, must be one of "two.sided" (default),
#?        "greater" or "less".
#' @param xvar a numeric value for the variance of the first group
#' @param yvar a numeric value for the variance of the second group
#' @param paired a logical indicating value whether you want
#?        a paired t-test
#' @export
#' @name TwoSamplesMeans
#' @return a list with 3 elements:
#' \item{statistic}{the value of the test statistic}
#' \item{df}{If it is available, the degree of freedom for the test statistic}
#' \item{p.value}{the p-value for the test}
#' \item{test}{a character string indicating which method was used}
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#'
#' x<-c(10, 25, 35, 40, 70, 60, 50, 70, 65, 25)
#' y<-c(30, 20, 60, 70, 50, 90, 80, 65, 75, 60)
#' TwoSamplesMeans(x = x,y = y,H1 = "two.sided")

TwoSamplesMeans<-function(x,y,
                          var.equal=FALSE,
                          H1="two.sided",
                          xvar=NULL,yvar=NULL,
                          paired=FALSE){
  if(paired==TRUE){
    d<-x-y
    if(stats::shapiro.test(d)$p<0.05){
      result<-stats::wilcox.test(x = x,y=y,
                                 paired=TRUE,
                                 alternative = H1,
                                 exact=FALSE)
    }else{
      result<-stats::t.test(x = x,y=y,paired=TRUE,
                            alternative = H1)
    }
  } else if(stats::shapiro.test(x)$p<0.05 |
            stats::shapiro.test(y)$p<0.05){
    result<-stats::wilcox.test(x = x,y=y,
                               alternative = H1,exact=FALSE)

  }else if(missing(xvar)==FALSE &
           missing(yvar)==FALSE){
    result<-BSDA::z.test(x = x, y=y,
                         sigma.x=sqrt(xvar),
                         sigma.y=sqrt(yvar),
                         alternative = H1)
    result$parameter=NULL

  } else {
    result<-stats::t.test(x = x,y=y,alternative = H1,
                   var.equal=var.equal)
  }
  return(list(statistic=round(result$statistic,3),
              df=result$parameter,
              p.value=round(result$p.value,3),
              method=result$method))
}
