
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




#' Get data of users who starred certain repository
#'
#' For a given repository url, return n x 17 dataframe of n users who starred the repositry
#'
#' @param repository.url, token
#' @return usernames of those who starred the repository
#' @export
getStargazersData=function(repository.url,token)
{

  n=getNumStargazers(repository.url,token)
  api.url=changeGitHubRepoURLtoGitHubRepoAPICall(repository.url)
  command=paste(api.url,"/stargazers?per_page=100&page=",sep="")
  pages.url=paste(command,(1:(n/100+1)),sep="")

  Stargazer.list=LimitConsciousExtraction("extract_info",pages.url,token)
  Stargazer.df=do.call(rbind.data.frame, Stargazer.list)
  Stargazer.df.final=as.data.frame(apply(Stargazer.df,2,unlist,use.names=FALSE),stringsAsFactors = FALSE)

  return(Stargazer.df.final)
}
