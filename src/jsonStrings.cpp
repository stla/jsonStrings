
#include <Rcpp.h>
#include "nlohmann/json.hpp"
using json = nlohmann::json;

// [[Rcpp::export]]
std::string jsonArrayAt(std::string jsonString, int i){
  json js = json::parse(jsonString);
  json elem = js.at(i);
  return elem.dump();
}

// [[Rcpp::export]]
std::string jsonObjectAt(std::string jsonString, std::string key){
  json js = json::parse(jsonString);
  json elem = js[key];
  return elem.dump();
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
