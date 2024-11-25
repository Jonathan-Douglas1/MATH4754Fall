# one sample t-test
#' Title
#'
#' @param mean_in - the mean to be confidence tested
#' @param mean - mean of the normal sample
#' @param sd - sd of normal sample
#' @param n -  size of sample
#' @importFrom graphics boxplot
#' @importFrom stats rnorm t.test
#' @return
#' @export
#'
#' @examples
find_int = function(mean_in, mean, sd, n){
  set.seed(55);
  x1=rnorm(n,mean= mean,sd= sd)
  boxplot(x1, main="Sample x1")

  #t.test(x1,mu=28)
  test=t.test(x1,mu=mean_in)
  ci = test$conf.int
  print(ci)
  cat("P value:",test$p.value)


  abline(h=c(ci,mean(x1)),col=c("Red","Red","Green"))
  abline(h=mean_in,col=c("black"))
}
