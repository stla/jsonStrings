
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

