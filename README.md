# MayInstitute2020-IntermediateR

Materials for May Institute 2020 Online - Intermediate R Crash Course

# Installation instructions

Install required packages for Intermediate R course section from CRAN and Bioconductor:

```
required_packages_CRAN <- c("ggplot2", "tidyverse", "pryr", "microbenchmark",
                            "roxygen2", "RUnit", "testthat", "BiocManager")
required_packages_BioC <- c("MSnbase", "MSstats", "Cardinal", "matter", "HDF5Array")

install.packages(required_packages_CRAN)

BiocManager::install(required_packages_BioC)
```

Test whether all required packages:

```
required_packages <- c(required_packages_CRAN, required_packages_BioC)
installed_packages <- required_packages %in% installed.packages()[,"Package"]
missing_packages <- required_packages[!installed_packages]

if ( length(missing_packages) > 0 ) {
	warning(sprintf('FOLLOWING PACKAGES NEED TO BE INSTALLED STILL:\n\t%s\n',
		paste(missing_packages, collapse=', ')))
} else{
	message('ALL PACKAGES ARE INSTALLED, WE\'RE GOOD TO GO!\n')
}
```
