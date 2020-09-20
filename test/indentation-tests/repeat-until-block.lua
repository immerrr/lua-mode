-- works for repeat ... until blocks: 1

repeat
   a = a + 1
until foo + bar

a = 0

-- works for repeat ... until blocks: 2

repeat
   a = a + 1
until
   foo

a = 0

-- works for repeat ... until blocks: 3

repeat
   a = a + 1
until
   not
   foo

a = 0

-- works for repeat ... until blocks: 4

repeat
   a =
      a + 1
until
   not
   foo

a = 0

-- works for repeat ... until blocks: single line

repeat a = a + 1 until not foo

a = 0

-- works for repeat ... until blocks: single line with continuation 1

repeat a = a + 1 until
   not foo

a = 0

-- XFAIL: works for repeat ... until blocks: single line with continuation 1

repeat a =
      a + 1 until not foo

a = 0
