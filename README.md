jsonStrings
================

<!-- badges: start -->
[![R-CMD-check](https://github.com/stla/jsonStrings/workflows/R-CMD-check/badge.svg)](https://github.com/stla/jsonStrings/actions)
<!-- badges: end -->

``` r
library(jsonStrings)
```

## Define a JSON string

``` r
jstring <- jsonString$new("
  {
    \"foo\": \"hello\",
    \"bar\": {\"x\": 1, \"y\": 2},
    \"baz\": [9, 99, null],
    \"qux\": [null, [0, 1], {\"a\": 1000}]
  }
")
```

## Extract a JSON value

``` r
jstring$at("foo")
## "hello"
jstring$at("bar", "y")
## 2
jstring$at("baz", 2)
## null
```

## Erase a JSON value

``` r
jstring$erase("baz")
jstring
## {
##     "bar": {
##         "x": 1,
##         "y": 2
##     },
##     "foo": "hello",
##     "qux": [
##         null,
##         [
##             0,
##             1
##         ],
##         {
##             "a": 1000
##         }
##     ]
## }
```

## Check existence of a property

``` r
jstring$hasKey("bar")
## [1] TRUE
```

## Check a type

``` r
jstring$is("object")
## [1] TRUE
```

## Add a property

``` r
jstring$addProperty("new", "[4,5]")
jstring
## {
##     "bar": {
##         "x": 1,
##         "y": 2
##     },
##     "foo": "hello",
##     "new": [
##         4,
##         5
##     ],
##     "qux": [
##         null,
##         [
##             0,
##             1
##         ],
##         {
##             "a": 1000
##         }
##     ]
## }
```

## Update a JSON string

``` r
jstring$update(
  "{
    \"foo\": \"goodbye\",
    \"quux\": 10000
  }"
)
jstring
## {
##     "bar": {
##         "x": 1,
##         "y": 2
##     },
##     "foo": "goodbye",
##     "new": [
##         4,
##         5
##     ],
##     "quux": 10000,
##     "qux": [
##         null,
##         [
##             0,
##             1
##         ],
##         {
##             "a": 1000
##         }
##     ]
## }
```

## Patch a JSON string

``` r
jspatch <- "[
  {\"op\": \"remove\", \"path\": \"/foo\"},
  {\"op\": \"replace\", \"path\": \"/qux/2\", \"value\": 9999}
]"
jstring$patch(jspatch)
## {
##     "bar": {
##         "x": 1,
##         "y": 2
##     },
##     "new": [
##         4,
##         5
##     ],
##     "quux": 10000,
##     "qux": [
##         null,
##         [
##             0,
##             1
##         ],
##         9999
##     ]
## }
```
