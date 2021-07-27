.onAttach <- function(...) {
  if (!exists(antgreens_opt_name, 1)) {
    load_antgreens_options()
  }
  else {
    packageStartupMessage(
      "... You have already has variable `", antgreens_opt_name, "` in .GlobalEnv."
    )
    packageStartupMessage("... So every options you set will remain the same.")
  }
}
