library(yaml)

#' Reads a directory full of files and reports the number of completely 
#' observed cases in each data file.
#' 
#' @param 'directory' a string indicating the location of YAML files
#' @param 'name' a string that defines begean of file names
#' @param 'id' a number range for defining which files should be read
#' 
#' @return a list of read files
yaml.read <- function(directory, name = "", sufix = ".policy", id = 1:10) {
  # Retrieve file list based on pattern
  pattern <- paste(name, sufix, sep = ".*")
  filenames <- list.files(directory, pattern = pattern)
  filepaths <- lapply(filenames[id], function(f) {paste(directory, f, sep = "/")})
  
  fileslist <- lapply(filepaths, yaml.load_file)
  return(fileslist)
}

#' Converts list of raw, unstructured data into list of data.frames.
#' 
#' @param 'files.list' a list of raw data
#' @param  'id' a number range for defining which element to convert
#' 
#' @return list of data.frames
yaml.to.frame <- function(files.list, id = 1:length(files.list)) {
  frames.list <- list()
  for(i in id) {
    raw.list <- files.list[[i]]
    df <- data.frame()
    for(j in 1:length(raw.list)) {
      evaluation <- raw.list[[j]]$evaluation
      steps <- raw.list[[j]]$steps
      
      evaluation.frame <- data.frame()
      for (k in 1:length(raw.list[[j]]$population)) {
        velocity <- raw.list[[j]]$population[[k]]$velocity
        policy <- raw.list[[j]]$population[[k]]$policy
        
        policy.frame <- data.frame(evaluation = evaluation,
                                       rank = k,
                                       velocity = velocity,
                                       steps = steps,
                                       policy = policy)
        evaluation.frame <-rbind(evaluation.frame, policy.frame)
      }
      df <- rbind(df, evaluation.frame)
    }
    frames.list[[i]] <- df
  }
  
  return(frames.list)
}