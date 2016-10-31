#' Get userdata for the given username(s)
#'
#' Get userdata for a given username(s)
#'
#' @param token
#' @return number of calls left
#' @export
getUserdata=function(username,token)
{
  base="https://api.github.com/users/"
  command=paste(base,username,sep="")
  data=LimitConsciousExtraction("extract_info",command,token)
  return(data)
}



#' Get company of the given user
#'
#'
#' @param username,token
#' @return company name
#' @export
getUserWork=function(username,token)
{

  username="pumpy7"
  temp=as.character(getUserdata(username,token)["company"])
  return(ifelse(temp!="NULL",temp,"NA"))

}



