#' Get the data of issues for the repository
#'
#' For a given repository url, return data of issues for the repository
#'
#' @param repository.url, token
#' @return nested list of all issues for the repository
#' @export
getIssueData=function(repository.url,token)
{


  token=token
  issue.url=paste(repository.url,"/issues",sep="")
  temp=try(readLines(issue.url))
  if(!inherits(temp,'try-error'))
  {
    parsed=XML::htmlParse(temp)

    n=sum(as.numeric(unlist(strsplit(split="Open|Closed",gsub("[,\n ]+","",XML::xpathSApply(parsed,"//div[@class='table-list-header-toggle states float-left pl-3']",xmlValue))))))
  }

  api.url=changeGitHubRepoURLtoGitHubRepoAPICall(repository.url)

  command=paste(api.url,"/issues/",sep="")
  issues.url=paste(command,1:n,sep="")

  issue.list=parallelsugar::mclapply(issues.url,extract_info,token=token)

  return(issue.list)
}




#' Clean up the issue data provided by getCommitData function
#'
#' The getIssueData function returns data in nasty nested list, so cleaning up is necessary
#' This function gets rid of unnecessary information and returns issue title, issue author,
#' issue label, issue state, issue assignee, issue milestone, issue comments, issue created date, issue body
#'
#' @param object created by getCommitData function
#' @return issue title, issue author, issue state, issue assignee, issue label, issue milestone, issue numComments, issue created date, issue body
#' @export
cleanUpIssueData=function(issueData)
{
  issue.title=unlist(sapply(issueData,function(x){x$title}))
  issue.author=unlist(sapply(issueData,function(x){x$user$login}))
  issue.state=unlist(sapply(issueData,function(x){x$state}))
  issue.labels= sapply(issueData, function(x){
           ifelse(length(unlist(x$labels$name))!=0,paste(unlist(x$labels$name),collapse=", "),"NA")
         }
  )
  issue.assignee=sapply(issueData, function(x){
    ifelse(length(x$assignee$login)!=0,x$assignee$login,"NA")
  }
  )
  issue.milestone=sapply(issueData, function(x){
    ifelse(length(x$milestone)!=0,x$milestone,"NA")
  }
  )

  issue.numComments=sapply(issueData, function(x){
    ifelse(length(x$comments)!=0,as.numeric(x$comments),"NA")
  }
  )

  issue.createdDate=sapply(issueData, function(x){
    ifelse(length(x$created_at)!=0,x$created_at,"NA")
  }
  )

  issue.bodyMessage=sapply(issueData, function(x){
    ifelse(length(x$body)!=0,x$body,"NA")
  }
  )




  return(data.frame(title=issue.title,
                    author=issue.author,
                    state=issue.state,
                    labels=issue.labels,
                    assignee=issue.assignee,
                    milestone=issue.milestone,
                    numComments=issue.numComments,
                    createdAt=issue.createdDate,
                    body=issue.bodyMessage,
                    stringsAsFactors = FALSE))
}

