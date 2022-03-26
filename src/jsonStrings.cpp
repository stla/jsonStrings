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


class JSON {
 public:
   json jsonString;
   jsonXptr ptr;
  JSON(std::string string_) 
    : jsonString(toJSONstring(string_)), ptr(jsonXptr(&jsonString, false)) {}
  JSON(jsonXptr ptr_)
    : jsonString(*(ptr_.get())), ptr(jsonXptr(&jsonString, false)) {}

  jsonXptr at(Rcpp::List path) {
    json js = jsonString;
    for(R_xlen_t i = 0; i < path.size(); i++) {
      Rcpp::RObject robj = path(i);
      int obj_type = robj.sexp_type();
      if(obj_type == 13){
        if(!js.is_array()){
          Rcpp::stop("Not an array.");
        }
        Rcpp::IntegerVector vindex = Rcpp::wrap(robj);
        if(vindex.length() != 1){
          Rcpp::stop("Invalid path.");
        }
        int index = vindex[0];
        if(index < 0){
          Rcpp::stop("Negative indices make no sense.");
        }
        if(index >= (int)(js.size())) {
          Rcpp::stop("Too large index.");
        }
        js = js.at(index);
      } else if(obj_type == 16){
        if(!js.is_object()) {
          Rcpp::stop("Not an object.");
        }
        Rcpp::StringVector vkey = Rcpp::wrap(robj);
        if(vkey.length() != 1){
          Rcpp::stop("Invalid path.");
        }
        Rcpp::String key = vkey[0];
        js = js[key];
      }else{
        Rcpp::stop("Invalid path.");
      }
    }
    return jsonXptr(new json(js), false);
  }
  
  bool hasKey(std::string key) {
    return jsonString.contains(key);
  }

  void addProperty(std::string key, jsonXptr pptyXptr) {
    if(!jsonString.is_object()) {
      Rcpp::stop("The reference JSON string is not an object.");
    }
    if(jsonString.contains(key)) {
      Rcpp::stop("New key already present.");
    };
    json ppty = *(pptyXptr.get());
    jsonString.emplace(key, ppty);
  }
  
  void eraseProperty(std::string key) {
    if(!jsonString.is_object()) {
      Rcpp::stop("The reference JSON string is not an object.");
    }
    jsonString.erase(key);
  }
  
  void eraseElement(size_t idx) {
    if(!jsonString.is_array()) {
      Rcpp::stop("The reference JSON string is not an array.");
    }
    if(idx >= jsonString.size()) {
      Rcpp::stop("Too large index.");
    }
    jsonString.erase(idx);
  }
  
  size_t size() {
    return jsonString.size();
  }
  
  void update(jsonXptr obj) {
    if(!jsonString.is_object()) {
      Rcpp::stop("The reference JSON string is not an object.");
    }
    json js2 = *(obj.get());
    if(!js2.is_object()) {
      Rcpp::stop("The other JSON string is not an object.");
    }
    jsonString.update(js2);
  }
  
  void push(jsonXptr elem) {
    if(!(jsonString.is_array() || jsonString.is_null())) {
      Rcpp::stop("The reference JSON string is not an array.");
    }
    json js2 = *(elem.get());
    jsonString.push_back(js2);
  }
  
  jsonXptr patch(jsonXptr jspatchptr) {
    if(!(jsonString.is_object() || jsonString.is_array())) {
      Rcpp::stop("The reference JSON string must be an object or an array.");
    }
    json jspatch = *(jspatchptr.get());
    if(!jspatch.is_array()) {
      Rcpp::stop("The `patch` JSON string is not an array.");
    }
    try {
      json jsresult = jsonString.patch(jspatch);
      return jsonXptr(new json(jsresult), false);
    } catch(json::exception& e) {
      Rcpp::stop(e.what());
    }
  }
  
  void merge(jsonXptr jspatchptr) {
    json jspatch = *(jspatchptr.get());
    jsonString.merge_patch(jspatch);
  }
  
  bool is(int type) {
    bool result = false;
    switch(type) {
    case 1:
      result = jsonString.is_array();
      break;
    case 2:
      result = jsonString.is_object();
      break;
    case 3:
      result = jsonString.is_number();
      break;
    case 4:
      result = jsonString.is_number_integer();
      break;
    case 5:
      result = jsonString.is_string();
      break;
    case 6:
      result = jsonString.is_null();
      break;
    case 7:
      result = jsonString.is_boolean();
      break;
    }
    return result;
  }
  
  std::string type() {
    return jsonString.type_name();
  }
  
  std::string asString(bool pretty = false) {
    std::string string;
    if(pretty) {
      string = jsonString.dump(4);
    } else {
      string = jsonString.dump();
    }
    return string;
  }
  
};

RCPP_MODULE(class_JSON) {
  using namespace Rcpp;
  class_<JSON>("JSON")
    .constructor<std::string>()
    .constructor<jsonXptr>()
    .field("ptr", &JSON::ptr)
    .method("at", &JSON::at)
    .method("hasKey", &JSON::hasKey)
    .method("addProperty", &JSON::addProperty)
    .method("eraseProperty", &JSON::eraseProperty)
    .method("eraseElement", &JSON::eraseElement)
    .method("update", &JSON::update)
    .method("is", &JSON::is)
    .method("type", &JSON::type)
    .method("push", &JSON::push)
    .method("size", &JSON::size)
    .method("patch", &JSON::patch)
    .method("merge", &JSON::merge);
}

