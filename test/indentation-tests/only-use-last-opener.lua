-- Local Variables:
-- lua-indent-close-paren-align: nil
-- lua-indent-only-use-last-opener: t
-- End:

-- XFAIL: one param, nested table on same line as opener

foobar({
   a, b, c
})

-- XFAIL: two params, nested table on same line as opener

foobar(a, {
   b,
   c
})

foobar({}, {
   b,
   c
})

-- XFAIL: two aligned params, nested table on next line

foobar({},
       {1, 2, 3})

-- XFAIL: two aligned table params, first has nested tables

foobar({{},
        {1, 2, 3}},
       {
          4,5,6
       })

foobar({{},
        {1, 2, 3}},
       {
          4,5,6
       }
)

-- XFAIL: one nested table containing another table

foobar({
   {4, 5, 6}
})

-- XFAIL: nested table with indentation: nested table on separate line

foobar(
   a,
   {
      b,
      c
   })

foobar(
   a,
   {
      b,
      c
   }
)

-- XFAIL: nested table with alignment: nested table on separate line

foobar(a,
       {
          b,
          c
       })

foobar(a,
       {
          b,
          c
       }
)

-- nested table with indentation: params after nested table

foobar(
   {
      a,
      b
   },
   c, d)

foobar(
   {
      a,
      b
   },
   c, d
)
