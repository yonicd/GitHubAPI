
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
#' @param orgs.url, token
#' @return URLs of all repositories of the organization
#' @export
#'
getOrgsRepoUrls=function(orgs.url,token)
{
  orgsData=getOrgsData(orgs.url,token)
  n=orgsData$public_repos
  command=paste(orgsData$repos_url,"?per_page=100&page=",sep="")
  repos.command=paste(command,(1:(n/100+1)),sep="")
  repos.url=LimitConsciousExtraction("extract_info",repos.command,token=token)
  return(as.character(unlist(sapply(unlist(repos.url,recursive = FALSE),function(x){x["html_url"]}))))

}


#' Return urls of 10 repositories with the most stars for the given organization
#'
#' Extracts number of stars for all repositores belonging to the organization then return the top 10
#'
#' @param orgs.url, token
#' @return ten repositories urls with the most stars
#' @export
#'
getOrgsTop10Repos=function(orgs.url,token)
{

  all.repos=getOrgsRepoUrls(orgs.url,token)
  numStars=unlist(sapply(all.repos,getNumStargazers,token))


  return(head(sort(numStars,decreasing = TRUE),10))
}

