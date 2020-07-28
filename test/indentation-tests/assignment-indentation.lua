-- ensure is sane

foo = 10

bar = 20

-- add continuation before =

foo
   = 10

bar = 20

-- add continuation after =

foo =
   10
bar = 20

-- XFAIL: continuation after comma: 1
foo,
   baz = 10, 20

bar = 20

-- continuation after comma: 2

foo, baz
   = 10, 20

bar = 20

-- XFAIL: continuation after comma: 3

foo, baz = 10,
   20

bar = 20

-- XFAIL: continuation after comma: 4

foo,
   baz =
   10, 20

-- XFAIL: continuation after comma: 5

foo, baz =
   10,
   20

bar = 20

-- continuation after "local": 1

local
   x = 5

-- XFAIL: continuation after "local": 2

local
   x,
   y = 10, 20

-- XFAIL: continuation after "local": 3

local
   x,
   y =
   10,
   20

-- continuation after "local": 4

local
   x = 5

-- XFAIL: it unindents close paren for arithmetical expression

a = (
   foo +
   bar
)

-- XFAIL: it unindents close paren for arithmetical expression: local

local a = (
   foo +
   bar
)

-- it unindents close paren for function call

a = myfunc(
   foo +
   bar
)

-- it unindents close paren for function call: local

local a = myfunc(
   foo +
   bar
)

-- it unindents close brace for table ctor

a = {
   foo,
   bar
}

-- it unindents close brace for table ctor: local

local a = {
   foo,
   bar
}

-- XFAIL: it unindents close bracket for indexing

a = myobj[
   foo +
   bar
]

-- XFAIL: it unindents close bracket for indexing: local

local a = myobj[
   foo +
   bar
]
