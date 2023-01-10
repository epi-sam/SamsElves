#' Detach all user-loaded packages
#'
#' Start with a clean environment, devoid of packages that may cause namespace
#' function conflicts (e.g. plyr & dplyr).  Runs in a loop to deal with
#' package dependencies while unloading.
#'
#' @return [nothing]
#'
#' @export
detach_user_packages <- function () {

  message("Detaching all user-attached packages and clearing namespace to prevent function conflicts")

  counter <- 0 # to prevent infinite loop
  num_user_loaded <- length(names(sessionInfo()$otherPkgs))
  num_still_attached <- length(names(sessionInfo()$loadedOnly))

  while (num_user_loaded > 0 | num_still_attached > 0) {
    counter <- counter + 1
    cat("\n", paste0(num_user_loaded, " user packages are still loaded"),
        "\n")
    cat(paste0(num_still_attached, " packages are still attached to namespace"),
        "\n", "\n")
    print(names(sessionInfo()$loadedOnly))
    print(names(sessionInfo()$otherPkgs))
    tryCatch(

      {
        invisible(suppressWarnings(suppressPackageStartupMessages(lapply(names(sessionInfo()$loadedOnly),
                                                                         require, character.only = TRUE))))
        invisible(to_detach <- paste0("package:", names(sessionInfo()$otherPkgs)))
        invisible(suppressWarnings(lapply(to_detach, detach,
                                          character.only = TRUE, unload = TRUE, force = TRUE)))

      },
      error = function(e){message("You may still have a package loaded/attached from from a non-standard library path")}
    )
    num_user_loaded <- length(names(sessionInfo()$otherPkgs))
    num_still_attached <- length(names(sessionInfo()$loadedOnly))
    if (counter >= 10) {
      message("Attached: ",  names(sessionInfo()$loadedOnly))
      message("Loaded: ", names(sessionInfo()$otherPkgs))
      stop("User-attached packages were not all detached, please check sessionInfo(). \n       Consider restarting with ctrl + shift + F10.\n       detach_user_packages ran ",
           counter, " times.")
      break
    }
  }

  message("Detached all user-attached packages and cleared namespace to prevent function conflicts.
          detach_user_packages ran ", counter, " times.")

}
