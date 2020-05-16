#!/bin/sh
acme -v amind500z.b
diff -s amind.prg original/a_mind_is_born.prg
cmp amind.prg original/a_mind_is_born.prg