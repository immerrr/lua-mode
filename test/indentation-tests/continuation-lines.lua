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


-- indentation in block-intros: while

while
   foo do
   a = a + 1
end

a = 0

-- indentation in block-intros: while 2

while
   foo
do
   a = a + 1
end

a = 0

-- indents expressions after return: basic

function myfunc()
   return
      123
end

-- indents expressions after return: function literal

function myfunc()
   return
      function() return 123 end
end

-- indents expressions after return: ellipsis

function myfunc(...)
   return
      ...
end

-- does not indents keywords after return: end

function myfunc()
   return
end

-- does not indents keywords after return: if/end

function myfunc()
   if true then
      return
   end
end

-- does not indents keywords after return: if/else

function myfunc()
   if true then
      return
   else
      print(123)
   end
end

-- does not indents keywords after return: if/elseif

function myfunc()
   if true then
      return
   elseif false then
      print(123)
   end
end

-- does not indents keywords after return: repeat/until

function myfunc()
   repeat
      return
   until true
end

-- does not indents keywords after return: semicolon 1

function myfunc()
   if true then
      return;
   end
end

-- does not indents keywords after return: semicolon 2

function myfunc()
   if true then
      return;
      hello_world() -- this is incorrect syntax, but it's fine
   else
      return
         hello_world()
   end
end
