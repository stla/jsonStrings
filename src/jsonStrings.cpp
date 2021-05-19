/*#define JSON_DIAGNOSTICS 1
#ifdef NDEBUG
# define NDEBUG_DISABLED
# undef NDEBUG
#endif*/

#include "nlohmann/json_cran.hpp"
using json = nlohmann::json;

#include <Rcpp.h>


Rcpp::XPtr<json> jsonPointer(json jsonObject){
  Rcpp::XPtr<json> ptr(new json(jsonObject), true);
  ptr.attr("class") = "jsonString";
  return ptr;
}

class JSON {
public:
  JSON(std::string jsonString_) : jsonString(jsonString_){}
  
  Rcpp::XPtr<json> toJSONstring(){
    if(!json::accept(jsonString)){
      Rcpp::stop("Invalid JSON string");
    }
    return jsonPointer(json::parse(jsonString));
  }

private:
  std::string jsonString;
};

RCPP_MODULE(jsonModule){
  using namespace Rcpp;
  class_<JSON>("JSON")
  .constructor<std::string>()
  .method("jsonPointer", &JSON::toJSONstring)
  ;
}

class JSONPTR {
public:
  JSONPTR(Rcpp::XPtr<json> jsonPTR_) : jsonPTR(jsonPTR_){}

  Rcpp::XPtr<json> at(
      std::vector<std::string> keys,
      Rcpp::IntegerVector indices,
      std::vector<bool> isIndex){
    json js = *jsonPTR;
    for(size_t i = 0; i < isIndex.size(); i++){
      if(isIndex[i]){
        if(!js.is_array()){
          Rcpp::stop("Not an array.");  
        }
        size_t index = indices[i];
        if(index >= js.size()){
          Rcpp::stop("Too large index.");
        }
        js = js.at(index);  
      }else{
        if(!js.is_object()){
          Rcpp::stop("Not an object.");  
        }
        js = js[keys[i]];
      }
    }
    return jsonPointer(js);
  }

  bool hasKey(std::string key){
    json js = *jsonPTR;
    return js.contains(key);
  }

  Rcpp::XPtr<json> addProperty(
      std::string key,
      Rcpp::XPtr<json> valuePTR){
    json js = *jsonPTR;
    if(!js.is_object()){
      Rcpp::stop("Not an object.");  
    }
    if(js.contains(key)){
      Rcpp::stop("New key already present.");  
    };
    json jsvalue = *valuePTR;
    js.emplace(key, jsvalue);
    return jsonPointer(js);
  }

  Rcpp::XPtr<json> eraseProperty(std::string key){
    json js = *jsonPTR;
    if(!js.is_object()){
      Rcpp::stop("Not an object.");  
    }
    js.erase(key);
    return jsonPointer(js);
  }

  Rcpp::XPtr<json> eraseElement(size_t idx){
    json js = *jsonPTR;
    if(!js.is_array()){
      Rcpp::stop("Not an array.");  
    }
    if(idx >= js.size()){
      Rcpp::stop("Too large index");  
    }
    js.erase(idx);
    return jsonPointer(js);
  }
  
  size_t size(){
    json js = *jsonPTR;
    return js.size();
  }
  
  Rcpp::XPtr<json> update(Rcpp::XPtr<json> obj){
    json js1 = *jsonPTR;  
    if(!js1.is_object()){
      Rcpp::stop("Not an object.");  
    }
    json js2 = *obj;
    if(!js2.is_object()){
      Rcpp::stop("Not an object.");  
    }
    js1.update(js2);
    return jsonPointer(js1);
  }

  Rcpp::XPtr<json> push(Rcpp::XPtr<json> elem){
    json js1 = *jsonPTR;  
    if(!(js1.is_array() || js1.is_null())){
      Rcpp::stop("Not an array.");  
    }
    json js2 = *elem;
    js1.push_back(js2);
    return jsonPointer(js1);
  }
  
  Rcpp::XPtr<json> patch(Rcpp::XPtr<json> jspatchptr){
    json jsdoc = *jsonPTR;
    if(!(jsdoc.is_object() || jsdoc.is_array())){
      Rcpp::stop("The `doc` JSON string must be an object or an array.");
    }
    json jspatch = *jspatchptr;
    if(!jspatch.is_array()){
      Rcpp::stop("The `patch` JSON string is not an array.");
    }
    try
    {
      json jsresult = jsdoc.patch(jspatch);
      return jsonPointer(jsresult);
    }
    catch(json::exception& e)
    {
      Rcpp::stop(e.what());
    }
  }
  
  Rcpp::XPtr<json> merge(Rcpp::XPtr<json> jspatchptr){
    json jsdoc = *jsonPTR;
    json jspatch = *jspatchptr;
    jsdoc.merge_patch(jspatch);
    return jsonPointer(jsdoc);
  }
  
  bool is(int type){
    json js = *jsonPTR;
    bool result = false;
    switch(type) {
    case 1:
      result = js.is_array();
      break;
    case 2:
      result = js.is_object();
      break;
    case 3:
      result = js.is_number();
      break;
    case 4:
      result = js.is_number_integer();
      break;
    case 5:
      result = js.is_string();
      break;
    case 6:
      result = js.is_null();
      break;
    case 7:
      result = js.is_boolean();
      break;
    }
    return result;
  }

  std::string type(){
    json js = *jsonPTR;
    return js.type_name();
  }
    
  std::string jsonString(bool pretty = false){ 
    json js = *jsonPTR;
    std::string jsonstring;
    if(pretty){
      jsonstring = js.dump(4);  
    }else{
      jsonstring = js.dump();  
    }
    return jsonstring;
  }

private:
  Rcpp::XPtr<json> jsonPTR;
} ;

RCPP_MODULE(jsonptrModule){
  using namespace Rcpp;
  class_<JSONPTR>("JSONPTR")
    .constructor<Rcpp::XPtr<json>>()
    .method("at", &JSONPTR::at)
    .method("hasKey", &JSONPTR::hasKey)
    .method("addProperty", &JSONPTR::addProperty)
    .method("jsonString", &JSONPTR::jsonString)
    .method("eraseProperty", &JSONPTR::eraseProperty)
    .method("eraseElement", &JSONPTR::eraseElement)
    .method("update", &JSONPTR::update)
    .method("is", &JSONPTR::is)
    .method("type", &JSONPTR::type)
    .method("push", &JSONPTR::push)
    .method("size", &JSONPTR::size)
    .method("patch", &JSONPTR::patch)
    .method("merge", &JSONPTR::merge)
  ;
}

/*
//typedef Rcpp::XPtr<JSON> jsonPointer ;

// [[Rcpp::export]]
Rcpp::XPtr<JSON> create_ptr( std::string jsonString ){
  return Rcpp::XPtr<JSON>( new JSON(jsonString) ) ;
}

// [[Rcpp::export]]
std::string jat(Rcpp::XPtr<JSON> obj, std::string key){
  json js = obj->toJSON() ;
  return js[key];
}
*/