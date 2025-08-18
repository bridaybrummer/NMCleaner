# zzz.R
# .onLoad hook to set conflict preferences when package is loaded
# This sets dplyr::filter to be preferred via the conflicted package.

.onLoad <- function(libname, pkgname) {
	if (requireNamespace("conflicted", quietly = TRUE)) {
		# prefer dplyr::filter for the common verb 'filter'
		conflicted::conflict_prefer("filter", "dplyr")
	}
}

