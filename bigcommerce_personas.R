bigcommerce_personas <- function() {

	# load libraries


	# set path to data input and output
	path <- file.path(Sys.getenv("HOME"), "Documents", "data")

	# load data
	dat <- read.delim(file.path(path, "input", "Survey Results.txt"), stringsAsFactors=FALSE)

}