#' Record uncommitted git changes
#'
#' @param CODE_ROOT [path] path to code root containing a .git folder
#'
#' @return [character] succinct git diff vector length 1 with newlines for
#'   console printing with `cat()`, `glue::glue()` or `message()`
#' @export
query_git_diff <- function(CODE_ROOT) {
  current_dir <- getwd() # save to reset at the end
  on.exit(setwd(current_dir))

  setwd(CODE_ROOT) # must set WD for system commands
  suppressWarnings(
    uncommitted_changes <- system("git diff HEAD | grep ^[@+-]", intern = T)
  )

  git_uncommitted <- if(length(uncommitted_changes)) {
    # collapse for intuitive display
    uncommitted_changes <- paste(uncommitted_changes, collapse = "\n")
    uncommitted_changes <- paste(uncommitted_changes, "\n")
  } else {
    NULL
  }
  return(git_uncommitted)
}

#' Stop progress if user has uncommitted changes
#'
#' @param git_uncommitted [character] output from query_git_diff function
#'
#' @return [NULL]
#' @export
assert_git_diff <- function(git_uncommitted) {
  if (!is.null(git_uncommitted)) {
    cat(git_uncommitted)
    stop("You have uncommitted changes to your code.  Please commit and rerun.")
  } else {
    message("Passing git diff check.")
  }
}

#' Assert that daughter scripts share a common code state with launch script
#'
#' @param launch_hash [character] launch script hash as gold standard
#' @param script_hash [character] daughter script hash to check against main
#'
#' @return passing statement or error message
#'
#' @export
assert_git_hash <- function(launch_hash, script_hash) {

  if(is.null(launch_hash)) stop("Your upstream launch script hash is null.")
  if(is.null(script_hash)) stop("Your downstream script hash is null.")

  if (
    length(script_hash) != 1 ||
    !is.character(script_hash) ||
    length(launch_hash) != 1 ||
    !is.character(launch_hash)
  ) {
    stop("You must submit exactly one character string per hash.")
  }

  hashes_equal <- isTRUE(all.equal(launch_hash, script_hash))

  if(!hashes_equal){
    stop(
      paste(
        "Launch script git hash does not match downstream script git hash - please inspect and do a clean run.", "\n",
        "Launch hash = ", substr(launch_hash, 1, 8), "\n",
        "Script hash = ", substr(script_hash, 1, 8), "\n"
      )
    )
  }

}
