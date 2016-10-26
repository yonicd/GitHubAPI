
#' Get the number of forks for a repository.
#'
#' For a given repository url, return the number of forks for the respository
#'
#' @param repository.url, token
#' @return The number of forks for a particular repository
#' @export
getNumForks=function(repository.url,token)
{

  repo=try(extract_info(changeGitHubRepoURLtoGitHubRepoAPICall(repository.url),token))

  return(repo$forks_count)

}




#' Get data of users who forked certain repository
#'
#' For a given repository url, return n x 69 dataframe of n users who forked the repositry
#'
#' @param repository.url, token
#' @return usernames of those who forked the repository
#' @export
getForkData=function(repository.url,token)
{

  n=getNumForks(repository.url,token)
  api.url=changeGitHubRepoURLtoGitHubRepoAPICall(repository.url)
  command=paste(api.url,"/forks?per_page=100&page=",sep="")
  pages.url=paste(command,(1:(n/100+1)),sep="")

  fork.list=LimitConsciousExtraction("extract_info",pages.url,token)
  fork.df=do.call(rbind.data.frame, fork.list)
  fork.df.final=as.data.frame(apply(fork.df,2,unlist,use.names=FALSE),stringsAsFactors = FALSE)

  return(fork.df.final)
}
