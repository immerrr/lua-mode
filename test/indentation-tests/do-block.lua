-- works for do ... end blocks on separate lines
do
   a = a + 1
end

a = 0

-- works for do ... end blocks: single line

do a = a + 1 end

a = 0

-- works for do ... end blocks: body on the same line
do a = a + 1
end

a = 0

-- works for do ... end blocks: continuation inside body
do a = a
      + 1 end

a = 0

-- works for do ... end blocks: parentheses inside body
do a = (a
        + 1) end

a = 0
