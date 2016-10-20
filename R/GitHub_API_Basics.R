devtools::use_package("httr")
devtools::use_package("jsonlite")
devtools::use_package("parallelsugar")


#'
#' Create a GitHub oauth2.0 token
#'
#' This function takes clientID and clientSecret as input and create a GitHub token.
#'
#' @param clientID, clientSecret
#' @return GitHub oauth2.0 Token
#' @export
gitHub_Token_Generator=function(clientID,clientSecret)
{
  myapi=httr::oauth_app("github",clientID,clientSecret)
  github_token=httr::oauth2.0_token(httr::oauth_endpoints("github"),myapi)
  return(github_token)
}



#' Using a GitHub oauth2.0 token, execute an API call
#'
#' @param apiCall, token
#' @return API call output
#' @export

extract_info=function(apiCall,token)
{
  request=try(httr::GET(apiCall,httr::config(token=token)))
  if(!inherits(request,'try-error'))
  {
    request.content=httr::content(request)
    content=jsonlite::fromJSON(jsonlite::toJSON(request.content))
    return(content)
  }
  return(request)
}


#' Change the url of a repository to API url of the repository.
#'
#' For example, when running this function with https://github.com/octocat/Hello-World as input would return,
#' https://api.github.com/repos/octocat/Hello-World
#'
#' @param repository url
#' @return repository's API url
#' @export
changeGitHubRepoURLtoGitHubRepoAPICall=function(url)
{
  return(paste("https://api.github.com/repos/",paste(tail(unlist(strsplit(url,split="/")),2),collapse = "/"),sep=""))

}

