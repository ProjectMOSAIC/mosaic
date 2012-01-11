if (FALSE) {

.mosaic.get.ci = function( vals, level, method ) {
    if( method == "stderr" ) res = mean(vals) + c(-1,1)*sd(vals)*qt(1-(1-level)/2, length(vals)-1)
    else res = qdata( c((1-level)/2, 1-(1-level)/2), vals )
    return(res)
}
# ==================
confint.numeric = function(object, parm, level=0.95, method=c("stderr", "quantile"),margin=FALSE) {
  vals = .mosaic.get.ci( object, level, method[1] )
  if( margin ) return( c(point=mean(vals), margin.of.error=diff(vals)/2) )
  else return(vals)
}
# =================
confint.data.frame = function(object, parm, level=0.95, method=c("stderr", "quantile"), margin=FALSE) {
  nms = names(object)
  n = length(nms)
  res = data.frame( name=rep(NA,n), lower=rep(NA,n), upper=rep(NA,n))
  for (k in 1:n ) {
    if (is.numeric( object[[nms[k]]] )) {
      vals = .mosaic.get.ci( object[[nms[k]]], level, method[1])
      res$name[k] = nms[k]
      res$lower[k] = vals[1]
      res$upper[k] = vals[2]
    }
  }
  res = subset(res, !is.na(lower) )
  if( margin ) {
    res = data.frame(name=res$name, point=(res$upper+res$lower)/2, margin.of.error=(res$upper-res$lower)/2)
  }

  # Change the names to those given by confint.default
  colnames(res) = c("name", paste(c((1-level)/2, 1-(1-level)/2)*100, "%" ))
  return( res )
}
    
 
}
