library(tools)

all_files <- list.files(recursive = TRUE)
problematic_files <- character()

for (file in all_files) {
  tryCatch({
    # Read file in binary mode
    con <- file(file, "rb")
    raw_content <- readBin(con, what = "raw", n = file.info(file)$size)
    close(con)

    # Check for NUL characters
    if (any(raw_content == 0x00)) {
      cat("File with NUL characters:", file, "\n")
      problematic_files <- c(problematic_files, file)
    }
  }, error = function(e) {
    cat("Error reading file:", file, "\n")
    print(e)
  })
}

# Print list of problematic files
print(problematic_files)
