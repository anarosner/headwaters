## ------------------------------------------------------------------------
#  goodness of fit/validation functions
# library(topmodel)
# library(USGSwsStats)


## ----goodness of fit/validation functions--------------------------------

#' Calculate coefficient of variation of error
#' @param Calculates CV of error, given 2 vectors of observed and predicted values
#' @keywords cv, goodness of fit
#' @export
#' @examples
#' cv.error()
cv.error<-function(obs,pred) {
     sd(pred-obs)/mean(obs)
}


## ------------------------------------------------------------------------
#' Calculate model bias
#' @param Calculates model bias, given 2 vectors of observed and predicted values
#' @keywords bias, goodness of fit
#' @export
#' @examples
#' bias()
bias<-function(obs,pred) {
     b<-mean(pred)-mean(obs)
     return(list(bias=b,percent.bias=b/mean(obs)*100))
}


## ------------------------------------------------------------------------
#' Some goodness of fit, validation metrics 
#' @param Calculates and prints a variety of goodness of fit, validation metrics, including rmse, NS eff, bias, percent bias, pearson's R, and cv of error
#' @keywords goodness of fit
#' @export
#' @examples
#' goodness()
goodness<-function(df=NULL,obs=NULL,pred=NULL) {
     if(is.null(obs) | is.null(pred)){
          if(is.null(df)){
               print("ERROR: must define obs and pred vectors, 
                     or data frame w/ obs and pred columns")
               return(NULL)
          }
          else{
               obs<-df$obs
               pred<-df$pred
          }
     }
     f<-data.frame(sample.n=length(obs),mean=mean(obs))
     f$RMSE<-rmse(obs,pred)
     f$NSEff<-NSeff(obs,pred)
     f$bias<-mean(pred)-mean(obs)
     f$percent.bias<-(mean(pred)-mean(obs))/mean(obs)*100
     f$pearsonR<-cor(obs,pred,method="pearson")
     f$CV.error<-sd(pred-obs)/mean(obs)
     return(round(f,3))
}


