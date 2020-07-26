-- is sane
::foo::
::bar::

::baz::

a = 0

-- does not affect indentation when put on a separate line

for z=1,10 do
   ::foo::
   bar
end

a = 0

-- XFAIL: does not affect indentation before block modifiers

::foo:: for z=1,10 do
   bar
end

a = 0

-- does not affect indentation after block modifiers

for z=1,10 do  ::foo::
   bar
end

a = 0

-- reindents according to luawiki examples: 1

for z=1,10 do
   ::foo::
   for y=1,10 do  ::bar
      for x=1,10 do
         if x^2 + y^2 == z^2 then
            print('found a Pythagorean triple:', x, y, z)
            goto done
            goto done2
         end
      end
      ::done2::
   end
end

::done::

-- reindents according to luawiki examples: 2
for z=1,10 do
   for y=1,10 do
      for x=1,10 do
         if x^2 + y^2 == z^2 then
            print('found a Pythagorean triple:', x, y, z)
            print('now trying next z...')
            goto zcontinue
         end
      end
   end
   ::zcontinue::
end

-- reindents according to luawiki examples: 3

for x=1,5 do ::redo::
   print(x .. ' + 1 = ?')
   local y = tonumber(io.read'*l')
   if y ~= x + 1 then goto redo end
end

-- reindents according to luawiki examples: 4

::a::
print 'A'
if math.random() < 0.3 then goto c end
::b::
print 'B'
if math.random() < 0.5 then goto a end
::c::
print 'C'
if math.random() < 0.1 then goto a else goto b end

-- reindents according to luawiki examples: 5

function fact_(n, ans)
   ::call::
   if n == 0 then
      return ans
   else
      n, ans = n - 1, ans * n
      goto call
   end
end
print(fact_(5, 1)) --> 120

-- reindents according to luawiki examples: 6

function f()
   if not g() then goto fail end
   if not h() then goto cleanup_g end
   if not i() then goto cleanup_h end
   do return true end    -- need do/end?

   ::cleanup_h::
   undo_h()
   ::cleanup_g::
   undo_g()
   ::fail::
   return false
end

-- reindents according to luawiki examples: 7

::redo::
for x=1,10 do
   for y=1,10 do
      if not f(x,y) then goto continue end
      if not g(x,y) then goto skip end
      if not h(x,y) then goto redo end
      ::continue::
   end
end ::skip::
print('foo')
