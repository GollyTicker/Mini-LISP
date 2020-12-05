
const HTTP_REQUEST_TIMEOUT_MS = 3000
const serverAddr = "localhost"
const miniLISPInterpreterURL = "http://"+serverAddr+":3000/MiniLISP/file"
var mostRecentRequest = null;

const HTTPRequest = function (type, url, callback) {
  var request = new XMLHttpRequest();
  console.log("Request: " + url)
  request.open(type /*'GET'*/, url /*'/some-url'*/, true);
  request.timeout = HTTP_REQUEST_TIMEOUT_MS
  request.onreadystatechange = function(){
    if(request.readyState == 4){
      callback(request)
    }
  }
  request.send();
}

const formatGETQuery = function(url, key, rawValue){
  return url + "?" + key + "=" + encodeURIComponent(rawValue)
}

const ensureAborted = function(request) {
  request && request.abort()
}

export default {
  requestBackendEvaluation(text, responseHandler, errorHandler) {
    const URLwithQuery = formatGETQuery(miniLISPInterpreterURL, "value", text)
    const httpResponseHandler = function(httpResponse) {
      console.log("Received response ["+httpResponse.status+"].")
      if (httpResponse.status == 200) { responseHandler(httpResponse.responseText) }
      else { errorHandler() }
    }
    ensureAborted(mostRecentRequest)
    mostRecentRequest = HTTPRequest("GET",URLwithQuery,httpResponseHandler)
  }
}
