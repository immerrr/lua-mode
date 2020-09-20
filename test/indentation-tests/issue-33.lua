-- don't accumulate indentation after the expression
a =
   {
   }

b =
   {
   }

a = {
   table_elt_indented
}

a = a +
   5 +
   10

this_should_be_unindented()

-- here foobar should be indented as simple continuation statement
a = a +
   dosmth(
   ) +
   foobar

a =
   do_smth(
      do_smth_arg
   )

b =
   {
      table_elt0_indented,
      table_elt1_indented
   }

this_should_be_unindented_too =
   {
   }

this_should_be_unindented_three = etc
