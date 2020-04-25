

source(here::here("scr", "model", "run.R"))

source(here::here("scr", "lib", "IrishRulesModelSensitive.R"))

RunModel <- function(x, ir_run = FALSE, ir_def_run = FALSE, model_parameters = "default", run_type=run_type ) {
  # run_type can be "model" (outbreaks ) or "wth" (for weather data)
  # eval_run will parametarise model with predetermined set of parameters
  # ir_run  determine if the Irish Rules model is to be run and attached to the data. 
  y <- BlightR(x, run_type = run_type, model_parameters = model_parameters)
  
  if (ir_run == TRUE) {
    y$ir_risk <-
      IrishRulesModel(x,
                      temporal_res = "daily",
                      param = "modified",
                      replace_na = TRUE)
  }
  if (ir_def_run == TRUE) {
    y$defir_risk <-
      IrishRulesModel(x,
                      temporal_res = "daily",
                      param = "default",
                      replace_na = TRUE)
  }
  y <- y[2:c(nrow(y) - 1),]
  
  return(y)
}



cl <- makeCluster(detectCores())
clusterExport(cl, c("BlightR","IrishRulesModel", "RunModel", "ExtractCol"))

clusterEvalQ(cl, library("tidyverse", quietly = TRUE, verbose = FALSE))




out_ls <-
  pblapply(data_ls, function(x)
    RunModel(
      x,
      run_type = "fore",
      ir_run = TRUE,
      ir_def_run = TRUE
    ) , cl = cl)




# save(out_ls, file =here::here("out", "fore", "fore_model_out.Rdata"))
load( file =here::here("out", "fore", "fore_model_out.Rdata"))
