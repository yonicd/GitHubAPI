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


#' Get the number of stargazers for a repository.
#'
#' For a given repository url, return the number of stargazers for a respository
#'
#' @param repository.url, token
#' @return The number of stargazers for a particular repository
#' @export

getNumStargazers=function(repository.url,token)
{

  repo=extract_info(changeGitHubRepoURLtoGitHubRepoAPICall(repository.url),token)

  return(repo$stargazers_count)

}

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

#' Get the names of users who starred certain repository
#'
#' For a given repository url, return the usernames of those who starred the repositry
#'
#' @param repository.url, token
#' @return usernames of those who starred the repository
#' @export

getStargazersUsername=function(repository.url,token)
{
  n=getNumStargazers(repository.url,token)

  api.url=changeGitHubRepoURLtoGitHubRepoAPICall(repository.url)
  command=paste(api.url,"/stargazers?per_page=100&page=",sep="")
  pages.url=paste(command,(1:(n/100+1)),sep="")
  temp=parallelsugar::mclapply(pages.url,extract_info,token=token)
  usernames=unlist(sapply(temp,function(x){x$login}))
  return(usernames)
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

  return(paste("https://api.github.com/repos/",paste(url%>%strsplit("/")%>%unlist%>%tail(2),collapse = "/"),sep=""))

}

