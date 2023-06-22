#' Find the root directory of a git repository.
#' 
#' Intended use is from primary launch script.
#'
#' @return [path] path to root folder
#' @export
#'
#' @examples
define_git_root <- function(){
  
  if(interactive()){
    git_root <- rprojroot::is_git_root$find_file()
  } else {
    stop("This is only reliable with interactive sessions - pass code_root to submitted jobs through arguments.")
  }
  
  message("Repo root is ", git_root)
  
  return(git_root)
}
