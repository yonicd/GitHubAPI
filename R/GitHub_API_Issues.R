#' Get the data of issues for the repository
#'
#' For a given repository url, return data of issues for the repository
#'
#' @param repository.url, token
#' @return nested list of all issues for the repository
#' @export
getIssueData=function(repository.url,token)
{
  issue.url=paste(repository.url,"/issues",sep="")
  temp=try(readLines(issue.url))
  if(!inherits(temp,'try-error'))
  {
    parsed=XML::htmlParse(temp)
    n=sum(as.numeric(unlist(strsplit(split="Open|Closed",gsub("[,\n ]+","",XML::xpathSApply(parsed,"//div[@class='table-list-header-toggle states float-left pl-3']",XML::xmlValue))))))
    api.url=changeGitHubRepoURLtoGitHubRepoAPICall(repository.url)
    command=paste(api.url,"/issues/",sep="")
    issues.url=paste(command,1:n,sep="")
    issue.list=LimitConsciousExtraction("extract_info",issues.url,token=token)
  }
  return(issue.list)
}




#' Clean up the issue data provided by getIssueData function
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

  issue.title=unlist(sapply(issueData, function(x){
    ifelse(!is.null(unlist(x["title"])),x["title"],"NA")
  }
  ))


  issue.author=sapply(issueData,function(x){x["user"]})
  issue.author.cleanedup=as.character(unlist(sapply(issue.author,function(x){
    ifelse(length(x["login"])>0,paste(x["login"],collapse = ", "),"NA")
  }
  )))
  rm(issue.author)


  issue.state=unlist(sapply(issueData, function(x){
    ifelse(!is.null(unlist(x["state"])),x["state"],"NA")
  }
  ))


  issue.labels= sapply(issueData,function(x){x["labels"]})
  issue.labels.cleanedup=as.character(unlist(sapply(issue.labels,function(x){
  ifelse(!is.null(unlist(x["name"])),paste(x["name"],collapse = ", "),"NA")
    }
  )))
  rm(issue.labels)



  issue.assignee=sapply(issueData, function(x){x["assignee"]})
  issue.assignee.cleanedup=as.character(unlist(sapply(issue.assignee,function(x){
    ifelse(!is.null(unlist(x["login"])),paste(x["login"],collapse = ", "),"NA")
  }
  )))
  rm(issue.assignee)



  issue.milestone=sapply(issueData,function(x){x["milestone"]})
  issue.milestone.cleanedup=as.character(unlist(sapply(issue.milestone,function(x){
    ifelse(!is.null(unlist(x["title"])),paste(x["title"],collapse = ", "),"NA")
  }
  )))
  rm(issue.milestone)



  issue.numComments=as.numeric(sapply(issueData, function(x){
    ifelse(as.numeric(unlist(x["comments"]))!=0,as.numeric(x["comments"]),0)
  }
  ))



  issue.createdDate=unlist(sapply(issueData, function(x){
    ifelse(!is.null(unlist(x["created_at"])),x["created_at"],"NA")
  }
  ))



  issue.bodyMessage=unlist(sapply(issueData, function(x){
    ifelse(!is.null(unlist(x["body"])),x["body"],"NA")

  }

  ))


  return(data.frame(title=issue.title,
                    author=issue.author.cleanedup,
                    state=issue.state,
                    labels=issue.labels.cleanedup,
                    assignee=issue.assignee.cleanedup,
                    milestone=issue.milestone.cleanedup,
                    numComments=issue.numComments,
                    createdAt=issue.createdDate,
                    body=issue.bodyMessage,
                    stringsAsFactors = FALSE))
}

