
#' Get API data of an organization
#'
#' Return data regarding the given organization url
#'
#' @param orgs.url, token
#' @return data regarding the given organiztion url
#' @export
#'
getOrgsData=function(orgs.url,token)
{
  orgs.name=tail(unlist(strsplit(orgs.url,"/")),1)
  api.url=paste("https://api.github.com/orgs/",orgs.name,sep="")
  temp=try(extract_info(api.url,token))
  return(temp)

}




#' Return urls of repositories under the specified organization
#'
#' All repositories of the
#'
#' @param repository.url, token
#' @return The number of forks for a particular repository
#' @export
#'
getOrgsRepoUrls=function(orgs.url,token)
{


  orgsData=getOrgsData(orgs.url,token)
  n=orgsData$public_repos
  command=paste(orgsData$repos_url,"?per_page=100&page=",sep="")
  repos.command=paste(command,(1:(n/100+1)),sep="")
  repos.url=LimitConsciousExtraction("extract_info",repos.command,token=token)


  return(as.character(unlist(sapply(repos.url,function(x){x["html_url"]}))))

}

