
# Setup generics
setGeneric("smoothNoise",
	function(object, ...) standardGeneric("smoothNoise"))

setGeneric("removeBaseline",
	function(object, ...) standardGeneric("removeBaseline"))

setGeneric("findPeaks",
	function(object, ...) standardGeneric("findPeaks"))


# Smooth mass spectra
setMethod("smoothNoise", "MassSpectrum",
	function(object, halfWindow = 2, ...) {
		s <- .gaussianFilter(object@intensity, halfWindow=halfWindow)
		object@intensity <- s
		if ( validObject(object) )
			object
	})

.gaussianFilter <- function(x, halfWindow, ...) {
	sd <- ((halfWindow * 2) + 1) / 4
	kernel <- dnorm((-halfWindow):halfWindow, sd=sd)
	x2 <- c(rep(x[1], halfWindow), x, rep(x[length(x)], halfWindow))
	convolve(x2, kernel, type="filter")
}

# Remove baseline from mass spectra
setMethod("removeBaseline", "MassSpectrum",
	function(object, ...) {
		b <- .estimateBaseline(object@intensity)
		object@intensity <- pmax(object@intensity - b, 0)
		if ( validObject(object) )
			object
	})

.estimateBaseline <- function(x, ...) {
	i <- locmax(-x, ...)
	b <- x[i]
	if ( i[1L] != 1L ) {
		b <- c(b[1L], b)
		i <- c(1L, i)
	}
	if ( i[length(i)] != length(x) ) {
		b <- c(b, b[length(b)])
		i <- c(i, length(x))
	}
	approx(i, b, seq_along(x))$y
}

# Find peaks in mass spectra
setMethod("findPeaks", "MassSpectrum",
	function(object, SNR = 6, ...) {
		p <- .findPeaks(object@intensity, SNR=SNR, ...)
		object@peaks <- p
		if ( validObject(object) )
			object
	})

.findPeaks <- function(x, SNR, ...) {
	p1 <- locmax(x, ...)
	i <- seq_along(x)
	noise <- supsmu(i, x)$y
	p2 <- which(x / noise > SNR)
	intersect(p1, p2)
}

# Plot a mass spectrum
setMethod("plot", c("MassSpectrum", "missing"),
	function(x,
		xlab = expression(italic(m/z)),
		ylab = expression(italic(Intensity)),
		type = if (isCentroided(x)) 'h' else 'l',
		col = "darkcyan",
		peaklabels = 10,
		add = FALSE, ...)
	{
		# set up blank canvas
		if ( !add ) {
			plot(range(mz(x)), range(intensity(x)),
				xlab=xlab, ylab=ylab, type='n', ...)
			# draw reference line
			abline(h=0, lwd=0.5)
		}
		# draw spectrum
		lines(mz(x), intensity(x), type=type, col=col, ...)
		p <- peaks(x)
		# draw peaks
		if ( length(p) > 0L ) {
			lines(mz(x)[p], intensity(x)[p],
				type='h', col="red", lwd=1.5)
			plab <- p[order(intensity(x)[p], decreasing=TRUE)]
			plab <- sort(head(plab, n=peaklabels))
			text(mz(x)[plab], intensity(x)[plab],
				labels=round(mz(x)[plab], digits=4),
				pos=3, offset=0.1, cex=0.5)
		}
	})

# Plot two mass spectra
setMethod("plot", c("MassSpectrum", "MassSpectrum"),
	function(x, y,
		xlab = expression(italic(m/z)),
		ylab = expression(italic(Intensity)),
		type = if (isCentroided(x)) 'h' else 'l',
		col = c("darkcyan", "darkred"),
		peaklabels = 10,
		add = FALSE, ...)
	{
		# set up blank canvas
		if ( !add ) {
			plot(range(mz(x), mz(y)), range(intensity(x), -intensity(y)),
				xlab=xlab, ylab=ylab, type='n', ...)
			# draw reference line
			abline(h=0, lwd=0.5)
		}
		# draw spectrum
		col <- rep_len(col, 2)
		lines(mz(x), intensity(x), type=type, col=col[1], ...)
		lines(mz(y), -intensity(y), type=type, col=col[2], ...)
		p1 <- peaks(x)
		p2 <- peaks(y)
		# draw peaks
		if ( length(p1) > 0L ) {
			lines(mz(x)[p1], intensity(x)[p1],
				type='h', col="red", lwd=1.5)
			p1lab <- p1[order(intensity(x)[p1], decreasing=TRUE)]
			p1lab <- sort(head(p1lab, n=peaklabels))
			text(mz(x)[p1lab], intensity(x)[p1lab],
				labels=round(mz(x)[p1lab], digits=4),
				pos=3, offset=0.1, cex=0.5)
		}
		if ( length(p2) > 0L ) {
			lines(mz(y)[p2], -intensity(y)[p2],
				type='h', col="red", lwd=1.5)
			p2lab <- p2[order(intensity(y)[p2], decreasing=TRUE)]
			p2lab <- sort(head(p2lab, n=peaklabels))
			text(mz(y)[p2lab], -intensity(y)[p2lab],
				labels=round(mz(y)[p2lab], digits=4),
				pos=1, offset=0.1, cex=0.5)
		}
	})


