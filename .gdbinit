set history save on
set trace-commands on
set logging on
set startup-with-shell off

define r0
  target remote :6666
  break mcursor_check
end
define n0
  #break normal_execute
  break normal.c:964
  #break getchar.c:295
  run
end
define n1
  display curwin->w_cursor
  display get_inserted()
end
