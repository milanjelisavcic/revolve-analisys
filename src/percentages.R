percentages <- function(df) {
  percent.df <- data.frame()
  for(c in seq(1, ncol(df), 2)){
    for(r in 1:nrow(df)) {
      percent.df[row.names(df[r,]),colnames(df)[c]] <- 
        round((df[r,c] - df[r,1]) / df[r,1], 2)
    }
  }
  return(percent.df)
}