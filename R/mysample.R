#' My Sample
#'
#' @param n sample size
#' @param iter the amount of times sample will be produced
#' @param time time interval between sample & barplot production
#'
#' @return iter number of samples and barplots
#' @export
#'
#' @examples mysample(n = 10, iter = 15, time = 1.5)
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
