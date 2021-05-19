
#include <Rcpp.h>
#include "nlohmann/json.hpp"
using json = nlohmann::json;

// [[Rcpp::export]]
bool cpp_jsonHasKey(std::string jsonString, std::string key){
  if(!json::accept(jsonString)){
    Rcpp::stop("Invalid JSON string");
  }
  json js = json::parse(jsonString);
  return js.contains(key);
}

// [[Rcpp::export]]
std::string cpp_jsonAt(
    std::string jsonString, 
    std::vector<std::string> keys,
    Rcpp::IntegerVector indices,
    std::vector<bool> isIndex){
  if(!json::accept(jsonString)){
    Rcpp::stop("Invalid JSON string");
  }
  json js = json::parse(jsonString);
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
  return js.dump();
}

// [[Rcpp::export]]
std::string cpp_jsonAddProperty(
    std::string jsonString, 
    std::string key,
    std::string value){
  if(!json::accept(jsonString)){
    Rcpp::stop("Invalid JSON string.");
  }
  if(!json::accept(value)){
    Rcpp::stop("Invalid JSON string (new value).");
  }
  json js = json::parse(jsonString);
  json jsvalue = json::parse(value);
  if(!js.is_object()){
    Rcpp::stop("Not an object.");  
  }
  if(js.contains(key)){
    Rcpp::stop("New key already present.");  
  };
  js.emplace(key, jsvalue);
  return js.dump();
}

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
  
  bool is(int type){
    json js = *jsonPTR;
    bool result;
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
  
  std::string jsonString(){ 
    json js = *jsonPTR;
    return js.dump();
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