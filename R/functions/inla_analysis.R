inla_analysis <- function(data, family, formula) {
  model <- inla(formula, 
                family = family, 
                data = data,
                control.compute=list(dic=TRUE,cpo=TRUE, waic = TRUE, 
                                     return.marginals.predictor = TRUE,
                                     config=TRUE,residuals = TRUE),
                control.predictor = list(compute = TRUE)) 
  model
}