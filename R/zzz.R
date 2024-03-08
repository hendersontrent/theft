
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", packageVersion(pkgname), 
                        " of ", pkgname, ". All analysis and visualisation functions (plus a host of new ones!) have been moved to the {theftdlc} package\nand subsequently renamed and refactored.\nYou can install {theftdlc} via install.packages('theftdlc').\nPlease see https://hendersontrent.github.io/theftdlc/ for more.")
}
