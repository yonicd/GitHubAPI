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







#' Put system into sleep until GitHub API rate limit has been refreshed
#'
#' Put system into sleep until GitHub API rate limit has been refreshed.
#'
#'
#' @param NONE
#' @return Puts system in to sleep
#' @export
sleep=function(token)
{
  rate=getRate(token)
  rateReset=as.POSIXct(rate$reset,origin="1970-01-01")
  sleepTime=as.numeric(rateReset-Sys.time())*60
  Sys.sleep(sleepTime)

}




#' Check Remaining API call limit
#'
#' Check Remaining API call limit.
#'
#'
#' @param token
#' @return The number of calls left for the given token
#' @export
getRate=function(token)
{
  return(httr::content(httr::GET("https://api.github.com/rate_limit",httr::config(token=token)))$rate)
}



#' data extraction using GitHub API conscious of the API call rate limit
#' due to GitHub's 5000 call per hour rate limit, data extraction that requires more than 5000 call can be cumbersome.
#' This function executes function while automatically accommodating the rate limit.
#'
#'
#' @param token, the name of function in characters, x
#' @return list
#' @export
LimitConsciousExtraction=function(FUN,vectors,token)
{
  startEndMatrix=divideBy5000(vectors,token)
  n=nrow(startEndMatrix)


  for(i in 1:n)
  {
    if(i!=1)
    {
      sleep(token)
    }

    start=startEndMatrix[i,1]
    end=startEndMatrix[i,2]
    temp=vectors[start:end]

    text=paste("result",i,"=","try(parallelsugar::mclapply(temp,",FUN,",token=token))",sep="")
    eval(parse(text=text))

  }
  eval(parse(text=paste("data=c(",paste("result",1:n,collapse=",",sep=""),")",sep="")))
  if(length(data)==1)
  {
    return(data[[1]])
  }



  return(data)
}




#' Divide the elments by 5000 accomodating GitHub's 5000 API call per hour policy
#'
#'
#' @param x : vector of elements, token
#' @return matrix which tells starting and ending position
#' @export
divideBy5000=function(x,token)
{
  remainingRate=getRate(token)$remaining
  n=length(x)


  numIteration=n%/%5000+as.numeric(n%%5000!=0)
  temp=matrix(nrow=numIteration,ncol=2)

  nextStart=0


    for(i in  1:numIteration)
    {
      if(i==1)
      {
        start=1
        end=min(remainingRate,n)
        nextStart=end+1
      } else if(i==numIteration) {
        start=nextStart
        end=n

      } else{
        start=nextStart
        end=start+5000-1
        nextStart=end+1
      }
      temp[i,1]=start
      temp[i,2]=end
    }
    return(temp)

}
