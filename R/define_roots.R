#' Find the root directory of a git repository.
#' 
#' Intended use is from primary launch script.
#'
#' @return [path] path to root folder
#' @export
#'
#' @examples
define_git_repo_root <- function(){
  INTERACTIVE <- rstudioapi::isAvailable()
  
  
  git_root_criterion <- rprojroot::is_git_root
  
  if(!INTERACTIVE) stop("This is only reliable with Rstudio interactive sessions - pass code_root to submitted jobs through arguments.")
  
  git_root <- rprojroot::find_root(git_root_criterion)
  
  message("Repo root is ", git_root)
  
  return(git_root)
}
