
fwInfo <- function(x, options=NULL) {
	fwp <- paste(fwPath(), 'gdalinfo', sep='')
	x <- .getFilename(x)
	outfile <- extension(rasterTmpFile(), '.txt')
	fullcall <- paste(fwp, options, shQuote(x))
	system(fullcall, intern=TRUE)
}

