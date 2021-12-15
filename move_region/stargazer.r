# these two functions are some modification for tables

mod_stargazer <- function(...){
  output <- capture.output(stargazer(...))
  output <- output[4:length(output)]
  cat(paste(output, collapse = "\n"), "\n")
}

resizebox.stargazer = function(..., tab.width = "!", tab.height = "!"
){
  require(stringr) 
  res = capture.output(
    mod_stargazer(...)
  )
  tab.width = tab.width
  tab.height = tab.height
  res = 
    prepend(res, "}", before = length(res))
  res = 
    c(res[1:str_which(res, "^\\\\begin\\{tabular\\}.*")-1],
      paste0("\\resizebox{",tab.width,"}{",tab.height,"}{%"),
      res[str_which(res, "^\\\\begin\\{tabular\\}.*"):length(res)]
    )
  cat(res, sep = "\n")
}
