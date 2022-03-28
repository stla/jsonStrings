json toJSONstring(std::string);

class JsonString {
 public:
  json jsonString;
  jsonXptr ptr;
  JsonString(std::string string_)
      : jsonString(toJSONstring(string_)), ptr(jsonXptr(&jsonString, false)) {}
  JsonString(Rcpp::XPtr<json> ptr_, int xxx)
      : jsonString(*(ptr_.get())), ptr(jsonXptr(&jsonString, false)) {}

  jsonXptr at(Rcpp::List path) {
    json js = jsonString;
    for(R_xlen_t i = 0; i < path.size(); i++) {
      Rcpp::RObject robj = path(i);
      int obj_type = robj.sexp_type();
      if(obj_type == 13 || obj_type == 14) {
        if(!js.is_array()) {
          Rcpp::stop("Not an array.");
        }
        int index;
        if(obj_type == 13) {
          Rcpp::IntegerVector vindex = Rcpp::wrap(robj);
          if(vindex.length() != 1) {
            Rcpp::stop("Invalid path.");
          }
          index = vindex[0];
        } else {
          Rcpp::NumericVector vindex = Rcpp::wrap(robj);
          if(vindex.length() != 1) {
            Rcpp::stop("Invalid path.");
          }
          index = (int)(vindex[0]);
        }
        if(index < 0) {
          Rcpp::stop("Negative indices make no sense.");
        }
        if(index >= (int)(js.size())) {
          Rcpp::stop("Too large index.");
        }
        js = js.at(index);
      } else if(obj_type == 16) {
        if(!js.is_object()) {
          Rcpp::stop("Not an object.");
        }
        Rcpp::StringVector vkey = Rcpp::wrap(robj);
        if(vkey.length() != 1) {
          Rcpp::stop("Invalid path.");
        }
        Rcpp::String key = vkey[0];
        js = js[key];
      } else {
        Rcpp::stop("Invalid path.");
      }
    }
    return jsonXptr(new json(js), false);
  }

  bool hasKey(std::string key) { return jsonString.contains(key); }

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

  size_t size() { return jsonString.size(); }

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

  bool is(std::string type) {
    std::array<std::string, 7> types = {"array",  "object", "number", "integer",
                                        "string", "null",   "boolean"};
    auto it = std::find(types.begin(), types.end(), type);
    if(it == types.end()) {
      Rcpp::stop("Unknown type.");
    }
    int index = it - types.begin() + 1;
    bool result = false;
    switch(index) {
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

  std::string type() { return jsonString.type_name(); }

  std::string asString(bool pretty = false) {
    std::string string;
    if(pretty) {
      string = jsonString.dump(4);
    } else {
      string = jsonString.dump();
    }
    return string;
  }

  void print(bool pretty = true) {
    Rcpp::Rcout << this->asString(pretty) << "\n";
  }
};