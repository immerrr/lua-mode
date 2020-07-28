-- works for for ... do block: 1

for k, v in pairs(bar) do
   a = a + 1
end

a = 0

-- works for for ... do block: 2

for
   k, v in pairs(bar) do
   a = a + 1
end

a = 0

-- XFAIL: works for for ... do block: 3

for
   k,
   v in pairs(bar) do
   a = a + 1
end

a = 0

-- XFAIL: works for for ... do block: 4

for
   k,
   v in
   pairs(bar) do
   a = a + 1
end

a = 0

-- works for for ... do block: 5

for k, v in
   pairs(bar) do
   a = a + 1
end

a = 0

-- works for for ... do block: 6

for k, v in
   pairs(bar)
do
   a = a + 1
end

a = 0

-- works for for ... do block: 7
for k, v in pairs(bar)
do
   a = a + 1
end

a = 0

-- works for for ... do block: single line
for k, v in pairs(bar) do a = a + 1 end

a = 0
