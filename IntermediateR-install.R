########################################################################################
## Install required packages for Intermediate R course section from CRAN and Bioconductor
required_packages_CRAN <- c("ggplot2", "tidyverse", "pryr", "microbenchmark",
                            "roxygen2", "RUnit", "testthat", "BiocManager")
required_packages_BioC <- c("MSnbase", "MSstats", "Cardinal", "matter", "HDF5Array")

install.packages(required_packages_CRAN)

BiocManager::install(required_packages_BioC)

########################################################################################
## Tests whether all required packages 
## Press the 'Source' button in the top right corner of this pane and check 
## whether the output in the Console pane confirms that all packages are installed

required_packages <- c(required_packages_CRAN, required_packages_BioC)
installed_packages <- required_packages %in% installed.packages()[,"Package"]
missing_packages <- required_packages[!installed_packages]

if ( length(missing_packages) > 0 ) {
	warning(sprintf('FOLLOWING PACKAGES NEED TO BE INSTALLED STILL:\n\t%s\n',
		paste(missing_packages, collapse=', ')))
} else{
	message('ALL PACKAGES ARE INSTALLED, WE\'RE GOOD TO GO!\n')
}

## NOTE: For examples using compiled code, you will also need command line tools:

## On Windows, please install Rtools (https://cran.r-project.org/bin/windows/Rtools/)
## On macOS, please install developer tools the https://cran.r-project.org/bin/macosx/tools/
