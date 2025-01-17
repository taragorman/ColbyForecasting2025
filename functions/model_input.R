read_model_input = function(scientificname = "Thunnus Thynnus",
                            mon = "Aug",
                            approach = "greedy",
                            path = data_path("model_input")){
  
  
  
  
  # Validate the approach input
  if (!approach %in% c("greedy", "conservative")) {
    stop("Approach must be one of 'greedy' or 'conservative'.")
  }
  
  # Create the filename based on the input arguments
  filename = paste0(scientificname, "_", mon, "_", approach, ".csv")
  
  # Construct the full file path
  filepath = file.path(path, filename)
  
  # Check if the file exists
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }
  
  # Read and return the data
  data = read.csv(filepath)
  return(data)
}

  

