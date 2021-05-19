
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

class JSON {
public:
  JSON( std::string jsonString_ ) : jsonString(jsonString_) {
//    array = new float[10] ;
  }
  
  Rcpp::XPtr<json> toJSON(){ return Rcpp::XPtr<json>(new json(json::parse(jsonString))) ; }
  // other methods doing stuff with the data
  
private:
  std::string jsonString;
} ;

RCPP_MODULE(json_module) {
  using namespace Rcpp;
  class_<JSON>( "JSON" )
  .constructor<std::string>()
  .method( "jsonPTR", &JSON::toJSON )
  ;
}
//RCPP_EXPOSED_AS(JSON)

class JSONPTR {
public:
  JSONPTR( Rcpp::XPtr<json> jsonPTR_ ) : jsonPTR(jsonPTR_) {}
  
  std::string getkey(std::string key){ 
    json x;
    x = *jsonPTR;
    json js = x[key] ; 
    return js.dump();
  }

private:
  Rcpp::XPtr<json> jsonPTR;
} ;

RCPP_MODULE(jsonptr_module) {
  using namespace Rcpp;
  class_<JSONPTR>( "JSONPTR" )
    .constructor<Rcpp::XPtr<json>>()
    .method( "getkey", &JSONPTR::getkey )
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