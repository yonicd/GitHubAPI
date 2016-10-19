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
  request=httr::GET(apiCall,config(token=token))
  request.content=httr::content(request)
  content=jsonlite::fromJSON(toJSON(request.content))

}








