-- works for if/then block: 1

if foo > bar then
   a = a + 1
end

a = 0

-- works for if/then block: 2

if
   foo > bar then
   a = a + 1
end

a = 0

-- works for if/then block: 3

if foo >
   bar then
   a = a + 1
end

a = 0

-- works for if/then block: 4

if foo > bar
then
   a = a + 1
end

a = 0

-- works for if/then block: 5

if
   foo > bar
then
   a = a + 1
end

a = 0

-- works for if/then block: 6

if foo >
   bar
then
   a = a + 1
end

a = 0

-- works for if/then block: single line 1

if foo then a = a + 1 end

a = 0

-- works for if/then block: single line 2

if foo
then a = a + 1 end

a = 0

-- works for if/then block: single line 3

if foo
then a = a + 1
end

a = 0

-- works for if/then block: single line 4

if foo
then a = a
      + 1
end

a = 0

-- works for if/else block: 1

if foo then
   a = a + 1
else
   a = a + 2
end

a = 0

-- works for if/else block: 2

if foo then a = a + 1 else
   a = a + 2
end

a = 0

-- works for if/else block: 3

if foo then a = a + 1
else
   a = a + 2
end

a = 0

-- works for if/else block: 4

if foo then
   a = a + 1
else a = a + 2 end

a = 0

-- works for if/else block: 5

if foo then
   a = a + 1
else a = a + 2
end

a = 0


-- works for if/else block: single line 1

if foo + bar then a = a + 1 else a = a + 2 end

a = 0

-- works for if/else block: single line 2

if foo + bar
then a = a + 1
else a = a + 2
end

a = 0

-- works for if/else block: single line 3

if foo + bar then a = a + 1
else a = a + 2
end

a = 0

-- works for if/else block: single line 4

if foo + bar
then a = a + 1 else a = a + 2
end

a = 0

-- XFAIL: works for if/else block: single line 5

if foo + bar
then a = a + 1 else a =
      a + 2 -- this line should be indented by 2 levels: else+continuation
end

a = 0

-- works for if/else block: single line 6

if foo
   + bar
then a =
      a + 1
else a =
      a + 2
end

a = 0


-- XFAIL: works for if/else block: parentheses in conditional

if (foo
    + bar) then a = a + 1 else
   a = a + 2
end

a = 0


-- works for if/elseif/else block: 1

if foo then
   a = a + 1
elseif bar then
   a = a + 2
elseif baz then
   a = a + 3
end

a = 0

-- works for if/elseif/else block: 2

if foo then a = a + 1 elseif bar then
   a = a + 2
elseif baz then
   a = a + 3
else
   a = a + 4
end

a = 0

-- XFAIL: works for if/elseif/else block: 3

if foo then
   a = a + 1
elseif bar then a = a + 2 elseif baz then
   a = a + 3
else
   a = a + 4
end

a = 0

-- XFAIL: works for if/elseif/else block: 4

if foo then
   a = a + 1
elseif bar then
   a = a + 2
elseif baz then a = a + 3 else
   a = a + 4
end

a = 0
