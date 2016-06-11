.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Welcome to tigerstats!\n",
      "To learn more about this package, consult its website:\n",
      "\thttp://homerhanumat.github.io/tigerstats"))
}