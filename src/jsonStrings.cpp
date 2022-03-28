#include "jsonStrings.h"

// jsonXptr jsonPointer(json jsonObject) {
//   jsonXptr ptr(new json(jsonObject), true);
//   ptr.attr("class") = "jsonStringPtr";
//   return ptr;
// }

json toJSONstring(std::string string) {
  if(!json::accept(string)) {
    Rcpp::stop("Invalid JSON string.");
  }
  return json::parse(string);
}

// [[Rcpp::export]]
Rcpp::XPtr<json> toJSONXptr(std::string string){
  json jstring = toJSONstring(string);
  return jsonXptr(new json(jstring), false);
}

#include "JsonString.h"

RCPP_MODULE(class_JsonString) {
  using namespace Rcpp;
  class_<JsonString>("JsonString")
      .constructor<std::string>()
      .constructor<XPtr<json>, int>()
      .field("ptr", &JsonString::ptr)
      .method("asString", &JsonString::asString)
      .method("print", &JsonString::print)
      .method("at", &JsonString::at)
      .method("hasKey", &JsonString::hasKey)
      .method("addProperty", &JsonString::addProperty)
      .method("eraseProperty", &JsonString::eraseProperty)
      .method("eraseElement", &JsonString::eraseElement)
      .method("update", &JsonString::update)
      .method("is", &JsonString::is)
      .method("type", &JsonString::type)
      .method("push", &JsonString::push)
      .method("size", &JsonString::size)
      .method("patch", &JsonString::patch)
      .method("merge", &JsonString::merge);
}