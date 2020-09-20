-- works for "for ... = ... do" block: 1
for y = 0, 10 do
   a = a + 1
end

a = 0

-- works for "for ... = ... do" block: 2
for y = 0, 10
do
   a = a + 1
end

a = 0

-- works for "for ... = ... do" block: 3
for y = 0,
   10 do
   a = a + 1
end

a = 0

-- works for "for ... = ... do" block: 4
for y = 0,
   10
do
   a = a + 1
end

a = 0

-- works for "for ... = ... do" block: 5
for y =
   0, 10 do
   a = a + 1
end

a = 0

-- works for "for ... = ... do" block: 6
for y =
   0, 10
do
   a = a + 1
end

a = 0

-- works for "for ... = ... do" block: 7
for y = foo(
   1,
   2),
   bar(3,
       4)
do
   a = a + 1
end

a = 0

-- works for "for ... = ... do" block: single line

for y = 0, 10 do a = a + 1 end

a = 0
