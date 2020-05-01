
# Simulate a mass spectrum
simSpectra <- function(n = 1L, peaks = 50L,
	mz = rlnorm(peaks, 7, 0.3), intensity = rlnorm(peaks, 1, 0.9),
	from = 0.9 * min(mz), to = 1.1 * max(mz), by = 100,
	sdpeaks = sdpeakmult * log1p(intensity), sdpeakmult = 0.2,
	sdnoise = 0.1, sdmz = 10, resolution = 1000, fmax = 0.5,
	baseline = 0, decay = 10, units=c("ppm", "mz"),
	representation = c("profile", "centroid"))
{
	if ( length(mz) != length(intensity) )
		stop("length of mz and intensity must match")
	units <- match.arg(units)
	representation <- match.arg(representation)
	if ( missing(mz) && (!missing(from) || !missing(to)) ) {
		mz <- (mz - min(mz)) / max(mz - min(mz))
		mz <- (from + 0.1 * (to - from)) + (0.8 * (to - from)) * mz
	}
	m <- switch(units,
		mz=seq(from=from, to=to, by=by),
		ppm=seq_ppm(from=from, to=to, ppm=2 * by))
	i <- order(mz)
	mz <- mz[i]
	intensity <- intensity[i]
	if ( n > 1L ) {
		replicate(n, simSpectra(mz=mz, intensity=intensity,
			from=from, to=to, by=by, sdpeaks=sdpeaks, sdpeakmult=sdpeakmult,
			sdnoise=sdnoise, sdmz=sdmz, resolution=resolution, fmax=fmax,
			baseline=baseline, decay=decay, units=units,
			representation=representation))
	} else {
		dmz <- mz / resolution
		sdwidth <- qnorm(1 - fmax / 2) * dmz
		sdpeaks <- rep_len(sdpeaks, peaks)
		mzerr <- rnorm(1) * switch(units, ppm=1e-6 * mz * sdmz, mz=sdmz)
		mzobs <- mz + mzerr
		b <- baseline * exp(-(decay/max(m)) * (m - min(m)))
		x <- .simSpectra(mzobs, intensity,
			peakwidth=sdwidth, sdpeaks=sdpeaks, sdnoise=sdnoise,
			mzrange=c(from, to), mzout=m)
		x <- pmax(b + x, 0)
		if ( representation == "centroid" ) {
			MassSpectrum(mz=mz, intensity=approx(m, x, mz)$y,
				isCentroided=TRUE)
		} else {
			MassSpectrum(mz=m, intensity=x,
				isCentroided=FALSE)
		}
	}
}

.simSpectra <- function(mz, intensity,
	peakwidth, sdpeaks, sdnoise, mzrange, mzout)
{
	x <- numeric(length(mzout))
	for ( i in seq_along(mz) ) {
		if ( intensity[i] <= 0 || mz[i] < mzrange[1] || mz[i] > mzrange[2] )
			next
		nearmz <- which(mz[i] - 6 * peakwidth[i] < mzout & mzout < mz[i] + 6 * peakwidth[i])
		xi <- dnorm(mzout[nearmz], mean=mz[i], sd=peakwidth[i])
		intensityerr <- rlnorm(1, sdlog=sdpeaks[i])
		intensityerr <- intensityerr - exp(sdpeaks[i]^2 / 2)
		yi <- intensity[i] + intensityerr
		x[nearmz] <- x[nearmz] + yi * (xi / max(xi))
	}
	noise <- rlnorm(length(x), sdlog=sdnoise)
	noise <- noise - exp(sdnoise^2 / 2)
	x + noise
}

# A sequence with half-bin-widths in ppm (parts-per-million)
seq_ppm <- function(from, to, ppm) {
	length.out <- (log(to) - log(from)) / log((1 + 1e-6 * ppm) / (1 - 1e-6 *ppm))
	length.out <- floor(1 + length.out)
	i <- seq_len(length.out)
	from * ((1 + 1e-6 * ppm) / (1 - 1e-6 * ppm))^(i-1)
}
