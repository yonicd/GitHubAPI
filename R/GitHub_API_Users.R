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

