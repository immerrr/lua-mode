## Indentation Tests

This directory contains indentation tests for `lua-mode`.

Each `*.lua` file will be processed by `test-indentation.el` as follows:

- the file itself will be added as a buttercup `describe` block

- each span of code between comments will be added as `it` block that checks that the code is reindented as given in the file

- last line of comment before Lua code span will be used as the name of `it` block

- spans of code that have empty name will be called `section 1`, `section 2` and so on

- spans of code that contain only whitespace will be skipped

- if the name of the code span contains `XFAIL`, the test will be created as an expected failure (`xit` in buttercup)

### Example

Here's an example:

```lua

-- function call indentation

function(
   1,
   2,
   3,
)

-- XXX: this is commented out for now
-- while block indentation
--
-- while true do
--    print(123)
-- end
--
-- function literal indentation

local x = function()
   return 1, 2, 3
end

```

It will create two tests, "function call indentation" and "function literal" indentation. The test called "while block indentation" will be ignored completely.

### Adding Configuration Parameters

To add configuration parameters use Emacs syntax for Local Variables:

```

-- Local Variables:
-- lua-indent-close-paren-align: nil
-- lua-indent-only-use-last-opener: t
-- End:

```

This can go anywhere in the file, but make sure that the code span after the local variables section has a name comment, otherwise it will use `End:` line as a name.
