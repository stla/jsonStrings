dllunload <- function(){
  dyn.unload(
    system.file("libs", "x64", "jsonStrings.dll", package = "jsonStrings")
  )
}

makedoc <- function(){
  roxygen2::roxygenise(load_code = roxygen2::load_installed)
}
