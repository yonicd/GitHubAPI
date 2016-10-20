
#' Get the number of forks for a repository.
#'
#' For a given repository url, return the number of forks for the respository
#'
#' @param repository.url, token
#' @return The number of forks for a particular repository
#' @export
getNumForks=function(repository.url,token)
{

  repo=extract_info(changeGitHubRepoURLtoGitHubRepoAPICall(repository.url),token)

  return(repo$forks_count)

}

