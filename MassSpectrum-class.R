
# Load required packages
require(S4Vectors)
require(ProtGenerics)

# MassSpectrum is a simple S4 class designed to
# encapsulate basic information about a mass spectrum.
setClass("MassSpectrum",
	contains = "Vector",
	slots = c(
		mz = "numeric",
		intensity = "numeric",
		isCentroided = "logical",
		peaks = "integer"))

.valid_MassSpectrum <- function(object) {
	errors <- NULL
	if ( is.unsorted(object@mz) )
		errors <- c(errors , "mz must be sorted in increasing order")
	if ( length(object@mz) != length(object@intensity) ) {
		errors <- c(errors , paste0("length of mz [",
			length(object@mz), "] must match length of intensity [",
			length(object@intensity), "]"))
	}
	outofrange <- object@peaks < 1L | object@peaks > length(object@mz)
	if ( length(object@peaks) > 0L && any(outofrange) ) {
		errors <- c(errors , paste0("out-of-range peak indices [",
			object@peaks[outofrange]))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("MassSpectrum", .valid_MassSpectrum)

# Create a MassSpectrum object.
MassSpectrum <- function(mz, intensity, isCentroided = FALSE) {
	if ( is.unsorted(mz) ) {
		i <- order(mz)
		intensity <- intensity[i]
		mz <- mz[i]
	}
	new("MassSpectrum", mz=mz, intensity=intensity,
		isCentroided=isCentroided, peaks=integer())
}

setMethod("mz", "MassSpectrum",
	function(object, ...) object@mz)

setReplaceMethod("mz", "MassSpectrum",
	function(object, value) {
		object@mz <- value
		if ( validObject(object) )
			object
	})

setMethod("intensity", "MassSpectrum",
	function(object, ...) object@intensity)

setMethod("peaks", "MassSpectrum",
	function(object, ...) object@peaks)

setMethod("isCentroided", "MassSpectrum",
	function(object, ...) object@isCentroided)

setMethod("tic", "MassSpectrum",
	function(object, ...) {
		sum(intensity(object))
	})

setMethod("length", "MassSpectrum",
	function(x) {
		length(mz(x))
	})

setMethod("[", "MassSpectrum",
	function(x, i, ...) {
		len <- length(x)
		if ( length(peaks(x)) > 0L ) {
			p <- replace(logical(len), peaks(x), TRUE)[i]
			x@peaks <- which(p)
		}
		x@mz <- x@mz[i]
		x@intensity <- x@intensity[i]
		if ( validObject(x) )
			x
	})

setMethod("show", "MassSpectrum",
	function(object) {
		cat("Mass Spectrum\n")
		mzr <- paste0(range(mz(object)), collapse=" to ")
		cat("  m/z range:", mzr, "\n")
		cat("  centroided:", isCentroided(object), "\n")
		cat("  length:", length(object), "\n")
		if ( length(peaks(object)) > 0L )
			cat("  num peaks:", length(peaks(object)), "\n")
		cat("  tic:", tic(object), "\n")
	})


