fit_inla <- function(data, 
                     formula, 
                     model, 
                     spatial = FALSE) {
  
  # models with spatial random effects
  if (spatial) {
    
    mod <- inla(formula,  
                data = data,  
                family = model, 
                control.fixed = list(mean=0, prec = 0.001))
  }
  
  #fit the default model without spatial random effect
  else {
    
    mod <- inla(formula,  
                data = data,  
                family = model, 
                control.fixed = list(mean=0, prec = 0.001))
  }
  
  mod
  
}
