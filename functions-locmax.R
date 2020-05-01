
# Return indices of local maxima
locmax <- function(x, halfWindow = 2, ..., version = 1) {
	switch(version,
		locmax1(x, halfWindow=halfWindow, ...),
		locmax2(x, halfWindow=halfWindow, ...),
		locmax3(x, halfWindow=halfWindow, ...))
}

# Return indices of local maxima (version 1)
locmax1 <- function(x, halfWindow = 2) {
	begin <- halfWindow + 1
	end <- length(x) - halfWindow
	out <- integer()
	if ( begin < 1L || end > length(x) )
		return(out)
	for ( i in begin:end ) {
		j1 <- i - halfWindow
		j2 <- i + halfWindow
		is_max <- TRUE
		for ( j in j1:j2 ) {
			if ( x[j] > x[i] )
				is_max <- FALSE
		}
		if ( is_max )
			out <- c(out, i)
	}
	out
}

# Return indices of local maxima (version 2)
locmax2 <- function(x, halfWindow = 2) {
	begin <- halfWindow + 1
	end <- length(x) - halfWindow
	if ( begin < 1L || end > length(x) )
		return(integer())
	out <- logical(length(x))
	for ( i in begin:end ) {
		j1 <- i - halfWindow
		j2 <- i + halfWindow
		out[i] <- TRUE
		for ( j in j1:j2 ) {
			if ( x[j] > x[i] ) {
				out[i] <- FALSE
				break
			}
		}
	}
	which(out)
}

# Return indices of local maxima (version 3)
locmax3 <- function(x, halfWindow = 2) {
	begin <- halfWindow + 1
	end <- length(x) - halfWindow
	if ( begin < 1L || end > length(x) )
		return(integer())
	out <- vapply(begin:end, function(i) {
		j1 <- i - halfWindow
		j2 <- i + halfWindow
		if ( any(x[j1:j2] > x[i]) ) {
			FALSE
		} else {
			TRUE
		}
	}, logical(1))
	out <- c(rep(FALSE, halfWindow),
		out, rep(FALSE, halfWindow))
	which(out)
}
