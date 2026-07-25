.onAttach <- function(libname, pkgname) {
  if (isTRUE(getOption("essurvey2.quiet_startup"))) {
    return(invisible())
  }

  # Silent when configured: a startup message that fires every session for
  # correctly set-up users is noise.
  if (!is.null(ess_user_id(error = FALSE))) {
    return(invisible())
  }

  packageStartupMessage(
    "essurvey2: no ESS API user ID is configured.\n",
    "\n",
    "  Every data download must carry a user ID. It is not a password -- the\n",
    "  ESS uses it to record download statistics.\n",
    "\n",
    "  1. Get yours by logging in at https://ess.sikt.no/en/api\n",
    "  2. Store it permanently by adding this line to your .Renviron file\n",
    "     (usethis::edit_r_environ() opens it), then restarting R:\n",
    "\n",
    "         ESS_USER_ID=your-user-id-here\n",
    "\n",
    "     A system environment variable of the same name works too, and is\n",
    "     the better choice on a shared or containerised machine.\n",
    "\n",
    "  For this session only: ess_set_user_id(\"your-user-id-here\")\n",
    "\n",
    "  Browsing functions such as ess_rounds() need no ID. See ?ess_user_id.\n",
    "  options(essurvey2.quiet_startup = TRUE) silences this message."
  )
}
