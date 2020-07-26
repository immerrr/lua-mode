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
