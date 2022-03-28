#include "jsonStrings.h"

json toJSONstring(std::string string) {
  if(!json::accept(string)) {
    Rcpp::stop("Invalid JSON string.");
  }
  return json::parse(string);
}

// [[Rcpp::export]]
jsonXptr toJSONXptr(std::string string){
  json jstring = toJSONstring(string);
  return jsonXptr(new json(jstring), false);
}

// [[Rcpp::export]]
jsonXptr read_json(std::string filename){
  std::ifstream i(filename);
  json jstring;
  i >> jstring;
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
      .method("keys", &JsonString::keys)
      .method("addProperty", &JsonString::addProperty)
      .method("eraseProperty", &JsonString::eraseProperty)
      .method("eraseElement", &JsonString::eraseElement)
      .method("update", &JsonString::update)
      .method("is", &JsonString::is)
      .method("type", &JsonString::type)
      .method("push", &JsonString::push)
      .method("size", &JsonString::size)
      .method("patch", &JsonString::patch)
      .method("merge", &JsonString::merge)
      .method("writeFile", &JsonString::writeFile);
}