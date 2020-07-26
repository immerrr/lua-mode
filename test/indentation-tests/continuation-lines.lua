-- indentation if broken in the middle of \"foo.bar\" and \"qux:quux\"
foo123
   .bar:baz(xyz)

foo123.
   bar:baz(xyz)

foo123.bar
   :baz(xyz)

foo123.bar:
   baz(xyz)

foo123.bar
   .baz
   .qux
   :quux(xyz)

-- indentation after return

function foo()
   return
      123
end

-- indentation after return: blocks

do
   return
      123
end

do
   return
      x +
      y
end

do
   return
end

foo = bar

-- indentation after return: f1

function f1()
   if foo == bar then
      return
   else
      foo = bar
   end
end

-- indentation after return: f2

function f2()
   if foo == bar then
      return
   elseif foo != bar then
      foo = bar
   end
end

-- indentation after return: f3

function f3()
   repeat
      return
   until foo == bar
end

-- indentation after ellipsis

function x(...)
   a, b = 1, ...
   return b
end


-- indentation of function call arguments in continuation part

x = foo(123,
        456)
   + bar(
      qux,
      quux)

-- does not indent binary operators inside parentheses: alignment 1

x = (very_very_very_long_name() +
     another_very_very_very_long_name())

-- does not indent binary operators inside parentheses: alignment 2

x = (very_very_very_long_name()
     + another_very_very_very_long_name())

-- does not indent binary operators inside parentheses: indentation 1

x = (
   very_very_very_long_name() +
   another_very_very_very_long_name()
    )

-- does not indent binary operators inside parentheses: indentation 2

x = (
   very_very_very_long_name()
   + another_very_very_very_long_name()
    )

-- does not indent binary operators inside brackets: alignment 1

x = t[very_very_very_long_name() +
      another_very_very_very_long_name()]

-- does not indent binary operators inside brackets: alignment 2

x = t[very_very_very_long_name()
      + another_very_very_very_long_name()]

-- does not indent binary operators inside brackets: indentation 1

x = [
   very_very_very_long_name() +
   another_very_very_very_long_name()
    ]

-- does not indent binary operators inside brackets: indentation 2

x = [
   very_very_very_long_name()
   + another_very_very_very_long_name()
    ]

-- XFAIL: indentation in block-intros: while

while
   foo do
   a = a + 1
end

a = 0

-- XFAIL: indentation in block-intros: for1

for k, v
   in pairs(bar) do
   a = a + 1
end

a = 0

-- XFAIL: indentation in block-intros: for2

for k, v
   in pairs(bar) do a = a + 1 end

a = 0
