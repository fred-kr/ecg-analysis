box::use(
  tools
)

#' @export
validate <- function(file) {
  valid_extensions <- c("fst", "rds", "csv", "txt")
  if (is.null(file)) {
    stop("Error: No file input provided.")
  } else if (!is.null(file$datapath)) {
    file_path <- file$datapath
    if (file.exists(file_path)) {
      if (file.info(file_path)$isdir) {
        stop("Error: The input file is a directory. Please provide a file path.")
      } else {
        file_ext <- tools$file_ext(file_path)
        if (!(file_ext %in% valid_extensions)) {
          stop(
            paste(
              "Error: The selected file must have one of the following extensions:",
              paste(valid_extensions, collapse = ", ")
            )
          )
        }
        return(file_path)
      }
    } else {
      stop("Error: The input file does not exist. Please provide a valid file path.")
    }
  } else {
    stop("Error: Invalid file input provided.")
  }
}
