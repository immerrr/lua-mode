-- THINGS TO TEST:
-- parentheses vs braces (table ctor)
-- close paren/brace: attached/detached
-- alignment/indentation
-- args in horizontal line vs args in column vs mixed
-- one of params is a table/function literal/function call/single-line table/single-line function

-- it works for single line case

foobar(a, b, c)

a = 0

-- it works for indenting all args on one line: close paren on separate line

foobar(
   a, b, c
)

a = 0

-- it works for indenting all args in a column: close paren on separate line

foobar(
   a,
   b,
   c
)

a = 0

-- it works for mixed arg indentation: close paren on separate line

foobar(
   a, b,
   c, d
)

a = 0

-- it works with table ctorfor single line case

foobar{a, b, c}

a = 0

-- it works with table ctor for indenting all args on one line: close paren on separate line

foobar{
   a, b, c
}

a = 0

-- it works with table ctor for indenting all args in a column: close paren on separate line

foobar{
   a,
   b,
   c
}

a = 0

-- it works with table ctor for mixed arg indentation: close paren on separate line

foobar{
   a, b,
   c, d
}

a = 0

-- it works for mixed arg indentation with table in the middle: close paren on separate line

foobar(
   a, b,
   {
      foo = bar,
      qux = quux
   }, d
)

a = 0

-- it works for mixed arg indentation with table first: close paren on separate line

foobar(
   {
      foo = bar,
      qux = quux
   }, b,
   c, d
)

a = 0

-- it works for mixed arg indentation with table last: close paren on separate line

foobar(
   a, b,
   c, {
      foo = bar,
      qux = quux
      }
)

a = 0

