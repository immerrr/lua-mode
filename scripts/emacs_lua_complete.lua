--    __emacs_lua_complete:
-- This function finds and prints a limited number of matches on the
-- lua globals tree, _G. See `lua-complete-string`. N.B.: Does not
-- complete numeric keys (e.g. for arrays) or keys which begin with an
-- underscore.
-- Arguments:
--   PARTS: a table array of dot-separated parts of the completion item
--     (e.g. table.con would become {"table","con"}).  All but the last
--     element must be valid subkeys in the _G heirarchy.
--   LIBS: an array of tables of the form
--       {var: varname, lib: libfile}
--     for e.g. local varname=require("libfile"). This library will be
--     require'd and searched alongside global variables.
--   LOCALS: an array of strings parsed from local aloc,bloc = ... to
--     complete as well
--   LIMIT: The maximum number of nodes to print.  Defaults to 2000.  
--
-- Example:
-- __emacs_lua_complete({'table','ins'},{{var="xyz",lib="libfile"}},
--                      {"frank","beans"},5000)
--
-- Written 2019/05 J.D. Smith
function __emacs_lua_complete(parts, libs, locals, limit)
   -- Setup a FIFO queue for breadth-first tree traversal
   local queue = {head = 0, tail = 0}
   function queue:push(value)
      self.tail = self.tail+1  self[self.tail] = value
   end
   function queue:pop()
      self.head = self.head+1  return self[self.head]
   end

   local cnt, limit=0, limit or 2000 -- max nodes to print

   -- Mirror the globals table
   local globals = {}
   for k,v in pairs(_G) do globals[k] = v end
   
   -- Add libs and locals
   if libs then
      for _,v in ipairs(libs) do
	 local good,lib=pcall(require,v.lib)
	 globals[v.var] = good and lib or {}
      end
   end
   if locals then
      for _,v in ipairs(locals) do globals[v] = {} end
   end

   -- Descend the tree to the limit of dotted parts
   local g, pre = globals, ""
   for i, part in ipairs(parts) do
      if not g then break end	-- should always exist
      if i < #parts then -- keep descending
	 pre = (i > 1 and pre.."." or "") .. part
	 g = g[part]
      else  -- final part; seed the queue with any matching tables
	 for k,v in pairs(g) do
	    if type(k) == "string" and
	       k:sub(1,#part) == part and
	       (part:sub(1,1) == "_" or k:sub(1,1) ~= "_")
	    then
	       local dpath=(i > 1 and pre .. "." or "") .. k
	       print(dpath)
	       cnt = cnt + 1
	       if type(v) == "table" then queue:push({pre = dpath,entries = v}) end
	    end
	 end
      end
   end

   -- Perform a limited breadth-first traversal of the queued tables
   while queue.head < queue.tail and cnt < limit do
      local q = queue:pop()
      for k,v in pairs(q.entries) do
	 if type(k) == "string" and k:sub(1,1) ~= "_" then 
	    pre = q.pre .. "." .. k
	    print(pre)
	    cnt = cnt + 1
	    if cnt >= limit then break end
	    if type(v) == "table" then queue:push({pre = pre,entries = v}) end
	 end
      end
   end
end
