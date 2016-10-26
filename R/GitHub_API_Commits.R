#' Get the data of commits for the repository
#'
#' For a given repository url, return data of commits of the repository
#'
#' @param repository.url, token
#' @return nested list of all commits for the repository
#' @export
getCommitData=function(repository.url,token)
{

  temp=try(readLines(repository.url))
  if(!inherits(temp,'try-error'))
  {
    parsed=XML::htmlParse(temp)
    n=as.numeric(gsub("[,\n ]+","",XML::xpathSApply(parsed,"//li[@class='commits']//span[@class='num text-emphasized']",XML::xmlValue)))
    api.url=changeGitHubRepoURLtoGitHubRepoAPICall(repository.url)
    command=paste(api.url,"/commits?per_page=100&page=",sep="")
    commitPages.url=paste(command,(1:(n/100+1)),sep="")
    commit.list=LimitConsciousExtraction("extract_info",commitPages.url,token=token)
  }
  return(commit.list)

}


#' Clean up the commit data provided by getCommitData function
#'
#' The getCommitData function returns data in nasty nested list, so cleaning up is necessary
#' This function gets rid of unnecessary information and returns commit sha, commit message, commit date
#' repository author's name, email and username and commiter's name,email and username.
#'
#' @param object created by getCommitData function
#' @return commit.cha, commit.message, commit.date, author.name, author.email, author.login, committer.name, committer.email, committer.login
#' @export
cleanUpCommitData=function(commitData)
{

  commit.sha=unlist(sapply(commitData,function(x){x$sha}))
  commit.message=unlist(sapply(commitData,
                               function(x){
                                  ifelse(sapply(x$commit$message,nchar)!=0,x$commit$message,"NA")
                                }
                          )   )

  commit.date=unlist(sapply(commitData,function(x){x$commit$committer$date}))
  author.name=unlist(sapply(commitData,function(x){x$commit$author$name}))
  author.email=unlist(sapply(commitData,function(x){x$commit$author$email}))
  author.login=unlist(sapply(commitData,
                             function(x){
                               ifelse(sapply(x$author$login,nchar)!=0,x$author$login,"NA")
                             }
  )   )

  committer.name=unlist(sapply(commitData,function(x){x$commit$committer$name}))
  committer.email=unlist(sapply(commitData,function(x){x$commit$committer$email}))
  committer.login=unlist(sapply(commitData,
                                function(x){
                                  ifelse(sapply(x$committer$login,nchar)!=0,x$committer$login,"NA")
                                }
  )   )


  return(data.frame(commit.sha=commit.sha,
                    commit.date=commit.date,
                    author.name=author.name,
                    author.email=author.email,
                    author.login=author.login,
                    committer.name=committer.name,
                    commiter.email=committer.email,
                    committer.login=committer.login,
                    commit.message=commit.message,
                    stringsAsFactors = FALSE))
}



