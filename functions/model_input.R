#' Reads a model input file given species, month, approach, and path
#' 
#' @param scientificname chr, the species name
#' @param mon chr month abbreviation
#' @param approach chr, one of "greedy" or "conservative"
#' @param path chr the path to the data directory
#' @return Data frame or list containing the model input data
#' @examples
#' # Example usage:
#' # read_model_input("Thunnus Thynnus", "Jan", "greedy", "path/to/data")
read_model_input <- function(scientificname = "Thunnus Thynnus",
                             mon = "Mar",
                             approach = "greedy",
                             path = data_path("model_input")) {
  if(FALSE){
    scientificname = "Thunnus Thynnus"
    mon = "Mar"
    approach = "greedy"
    path = data_path("model_input")
  }
  message("Starting function execution...")
  scientificname_split= strsplit(scientificname, split=" ")[[1]]
  first_word=gsub("_","_", scientificname_split[1])
  second_word= gsub("_", "_", scientificname_split[2])
  
  # Construct the file name
  file_name <- paste0(first_word,"_", second_word, "-", mon, "-", approach, "_input.gpkg")
  message("Constructed file name: ", file_name)
  
  # Construct the full file path
  file_path <- file.path(path, file_name)
  message("Constructed file path: ", file_path)
  
  # Check if file exists
  if (!file.exists(file_path)) {
    message(paste("File not found:", file_path))
    return (NULL)
    
  }
  else{
    message("File exists. Proceeding to read the file.")
    input_data <- st_read(file_path)
  }
  

  return(input_data)
}

  
  
  
  

  

