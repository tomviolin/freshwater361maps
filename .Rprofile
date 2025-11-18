#
# Freshwater 361 helpful utilities
#

loadlibrary <- function(package_name) {
  # Convert package_name to character if it's not already
  package_name_char <- as.character(substitute(package_name))

  # Check if the package is installed
  if (!requireNamespace(package_name_char, quietly = TRUE)) {
    message(paste("Package '", package_name_char, "' not found. Installing...", sep = ""))
    install.packages(package_name_char, dependencies = TRUE)
  }

  # Load the package
  library(package_name_char, character.only = TRUE)
}
