# for obtaining variables labels from ESS .sav files
get.ESS.label<-function(var){
  attr(var,which = "label")
}

# for combining weights to analogous inference to logistic regression
contrast.weights<-function(effects,group1,group2){
  
  d.eff<-data.frame(effects)
  d.eff$inv.var<-1/(d.eff$SE^2)
  
  group1.weights<-d.eff[group1,"inv.var"]
  group1.weights<-group1.weights/sum(group1.weights)
  group1.weights
  
  group2.weights<-d.eff[group2,"inv.var"]
  group2.weights<-group2.weights/sum(group2.weights)
  group2.weights
  
  ordering<-c(group1,group2)
  output<-c(group1.weights,-1*group2.weights)[ordering]
  return(output)
  
}

contrast.weights.total<-function(effects,signs){
  
  d.eff<-data.frame(effects)
  d.eff$inv.var<-1/(d.eff$SE^2)
  
  weights<-d.eff$inv.var/sum(d.eff$inv.var)
  
  output<-weights*signs
  return(output)
  
}

# variance inflation factor of predictors in multilevel models
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
