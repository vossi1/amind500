#!/bin/sh
acme -v amind500.b
diff -s amind500.prg original/a_mind_is_born.prg
cmp amind500.prg original/a_mind_is_born.prg