#' Wrange imaging output from Macros
#' 
#' This is a very specific function created for Chiara Volani and Irma Della Corte
#' to ease processing of multiple imaging macros
#'
#' @param inDir \code{character}. Where the function should read the files from
#' @param fileExtension \code{character}. The file extension. So far only `.csv` and `.tsv` are supported
#' @param outDir \code{character}. Where the function write the files to. default = `outputFiles`, will write into this newly created directory within `inDir`. NB: output file format will always be `.tsv` to avoid conflicts in file names.
#'
#' @returns A \code{list} with `data.frame`s, one per oucome of interest. This is returned only in case of an interactive session.
#' @export
#'
#' @examples
#' \dontrun{
#'   baseDir <- "path/to/git/repo"
#'   MACROS_wrangleData(inDir = "~/Documents/git_repos/WetLabUtils/testData/MACROS")
#'   MACROS_wrangleData(inDir = "~/Documents/git_repos/WetLabUtils/testData/MACROS", 
#'                      outDir = "~/Documents/git_repos/WetLabUtils/testData/MACROS/wrangledFiles")
#' }

MACROS_wrangleData <- function(inDir, fileExtension = ".csv", outDir = "outputFiles"){
  
  filePaths <- list.files(inDir, pattern = fileExtension, full.names = TRUE, recursive = FALSE)
  fileNames <- basename(filePaths)
  names(filePaths) <- fileNames
  
  # files metadata
  ## Plate well 
  plateWell <- regmatches(fileNames, regexpr("Well...", fileNames))
  plateWell <- sub("Well", "", plateWell)
  
  # reading series 
  series <- regmatches(fileNames, regexpr("series \\d+", fileNames))
  series <- factor(sub("series ", "", series))
  # get outcome raw input
  outcome <- regmatches(fileNames, regexpr("series \\d+.*", fileNames))
  outcome <- sub(".csv", replacement = "", x = outcome, fixed = TRUE) # remove file extension
  outcome_step2 <- sub("series (\\d+)_?", "", outcome) # remove series info
  
  fileNames_metadata.df <- 
    data.frame(
      fileName = fileNames,
      plateWell,
      series,
      outcome = outcome_step2
    )
  
  # split files per outcome of interest
  files_per_outcome <- split(filePaths, fileNames_metadata.df$outcome)
  
  # read files as list of data frames
  files_readIn.list <- lapply(files_per_outcome, function(x)
    lapply(names(x), function(fileName) {
      sep <- ifelse(fileExtension == ".csv", ",", "\t")
      tmp <- read.table(filePaths[fileName], row.names = 1, header = TRUE, sep = sep)
      # add file name to results
      tmp$fileName = fileName
      rownames(tmp) <- NULL
      # reorganize columns
      # tmp <- tmp[, c(ncol(tmp), 1:(ncol(tmp) - 1))]
      
      # merge experiment metadata
      tmp_merged <- merge(fileNames_metadata.df, tmp,by = "fileName", all.y = TRUE)
      return(tmp_merged)
    }))
  
  # merge files based on their experiment
  files_bound <- lapply(files_readIn.list, function(x) Reduce(rbind.data.frame, x))
  
  # fix a column name containing a special character
  files_bound <- lapply(files_bound, function(df){
    colnames(df)[colnames(df) == "X.Area"] <- "Percent_area"
    return(df)
  })
  
  
  # handle output directory instruction
  outDirOK <- file.path(inDir, outDir)
  dir.create(outDirOK, showWarnings = FALSE, recursive = TRUE)
  
  for(file in names(files_bound)){
    write.table(files_bound[[file]], file = paste0(outDirOK, "/", file, ".tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  }
  
  if(interactive()){
    return(files_bound)
  }
}
