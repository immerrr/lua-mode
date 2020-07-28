-- works for while ... do ... end blocks: 1

while foo + bar do
   a = a + 1
end

a = 0

-- works for while ... do ... end blocks: 2

while foo + bar
do
   a = a + 1
end

a = 0

-- works for while ... do ... end blocks: 3

while
   foo + bar do
   a = a + 1
end

a = 0

-- works for while ... do ... end blocks: 4

while foo +
   bar do
   a = a + 1
end

a = 0

-- works for while ... do ... end blocks: 5

while
   foo
   + bar
do
   a = a + 1
end

a = 0

-- works for while ... do ... end blocks: 6

while (
   foo) do
   a = a + 1
end

a = 0

-- works for while ... do ... end blocks: 7

while (
   foo
) do
   a = a + 1
end

a = 0

-- works for while ... do ... end blocks: 8

while (
   foo
)
do
   a = a + 1
end

a = 0

-- works for while ... do ... end blocks: single line

while foo + bar do a = a + 1 end

a = 0

-- works for while ... do ... end blocks: single line with continuation in body

while foo + bar do a = a +
      1 end

a = 0

-- works for while ... do ... end blocks: single line with parentheses in body

while foo + bar do a = (a +
                        1) end

a = 0
