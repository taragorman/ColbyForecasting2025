#' Make a prediction (for a year) given the sampling approach, scenario and year.
#' 
#' @param approach chr, one of "greedy" or "conservative"
#' @param scenario chr one of "PRESENT", "RCP45" or "RCP85"
#' @param year chr one of "2055" or "2075", ignored if `scenario` is "PRESENT"
#' @return stars predicted object
make_predictions = function(approach = "greedy",
                            scenario = "PRESENT",
                            year = "2055",
                            path = data_path("predictions")){
  
  if(FALSE){
    approach = "greedy"
    scenario = "PRESENT"
    year = "2055"
    path = data_path("predictions")
  }
  
  # make sure the path exists
  ok = make_path(path)
  
  #make sure the user passed correctly formed args
  scene = toupper(scenario[1])
  yr = if (scene == "PRESENT") {
    "PRESENT"
  } else {
    as.character(year[1])
  }
  DB = brickman_database()
  if (scene == "PRESENT"){
    DB = DB |>
      filter(scenario == scene, interval == "mon")
  } else {
    DB = DB |>
      filter(scenario == scene, year == yr, interval == "mon")
  }
  
  COVARS = read_brickman(DB)
  
  preds = sapply(month.abb,
                 function(mon_name){
                   version = sprintf("%s_%s", substr(approach, 1,1), mon_name)
                   cfg = try(read_configuration(version))
                   if (inherits(cfg, "try-error")){
                     return(NULL)
                   }
                   
                   wflow = try(read_workflow(version))
                   if (inherits(wflow, "try-error")){
                     return(NULL)
                   }
                   
                   covars = COVARS |>
                     select(cfg$keep_vars) |>
                     slice("month", mon_name)
                   
                   predfile = sprintf("%s_%s_%s.tif", version, scene, yr)
                   pred = predict_stars(wflow, covars) |>
                     write_prediction(file.path(path, predfile))
                   return(pred)
                 }, simplify = FALSE)
  
  ix = sapply(preds, is.null)
  if (all(!ix)){
    return(NULL)
  }
  preds = preds[!ix]
  nms = names(preds)
  
  p = do.call(c, append(preds, list(along = list(month = nms))))
  return(p)
}

