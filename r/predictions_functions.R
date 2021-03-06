
## ----predict-------------------------------------------------------------

predict.ts<-function( model.list, new.data, calib.data=NULL, dep.var.col=NULL, 
#                       selected.sites=F,
                      id.col="site_no", year.col="year", period.col="season", 
                      bias.correction=NULL, return.model.list=F ) {

     periods <- names( model.list )
     pred <- NULL
     
     for ( j in 1:length(periods) ) {
          if ( !is.null(calib.data) ) {
               season.calib.data <- calib.data[ calib.data[,period.col]==periods[[j]], ]
               model.list[[j]] <- update( model.list[[j]], data=season.calib.data )
          }
          
          season.new.data <- new.data[ new.data[,period.col]==periods[[j]], ]
          pred.temp <- season.new.data[ ,c(id.col, "date", period.col, year.col) ] 
          pred.temp$flow.log <- predict(  model.list[[j]], newdata=season.new.data  )
#           if ( (retransform) )
          pred.temp$flow.real <- retransform( pred.temp$flow.log, 
                                   bias.correction=bias.correction, residuals=residuals(model.list[[j]]) )
          
          if ( !is.null(dep.var.col) ) {
               if ( !is.null(calib.data) ) {
                    pred.temp$set <- "valid"
                    fitted.temp <- season.calib.data[ ,c(id.col, "date", period.col, year.col) ] 
                    fitted.temp$flow.log <- fitted.values(model.list[[j]])   
                    fitted.temp$flow.real <- retransform( fitted.temp$flow.log, 
                                              bias.correction=bias.correction, residuals=residuals(model.list[[j]]) )
                    fitted.temp$set <- "calib"
                    obs.temp <- rbind( season.calib.data[,c(id.col, "date", period.col, year.col)], 
                                       season.new.data[,c(id.col, "date", period.col, year.col)] )
                    obs.temp$flow.log <- log(c( season.calib.data[,dep.var.col], season.new.data[,dep.var.col] ))
                    obs.temp$flow.real <- c( season.calib.data[,dep.var.col], season.new.data[,dep.var.col] )
                    obs.temp$set <- "obs"
                    pred.temp <- rbind(pred.temp, fitted.temp, obs.temp)
               }
#                     fitted.temp[,dep.var.col] <- season.calib.data[,dep.var.col]
#                pred.temp[,dep.var.col] <- season.new.data[,dep.var.col]
          }       
          
          if( is.null(pred) )
               pred <- pred.temp
          else 
               pred <- rbind( pred, pred.temp )
     }
     
     pred <- pred[ order(pred$date), ]
#      if ( selected.sites ) {
#           pred$selected.site <- F
#           pred$selected.site[ pred[,id.col] %in% unique(new.data[,id.col ]) ] <- T
#      }

     if (!return.model.list)
          return( pred )     
     else 
          return(list( pred=pred, model.list=model.list ))
}




## ----retransform---------------------------------------------------------

retransform <- function( o, bias.correction=NULL, residuals=NULL, group="site_no" ) {
     
     if ( is.null(bias.correction) )
          return( exp(o) )
     else if ( bias.correction == "smearing" ) {
          corr <- mean(exp(residuals))
          return( exp(o)*corr )
     }
     else if ( bias.correction == "mvue" ) {
          return( NULL )
     }
}


