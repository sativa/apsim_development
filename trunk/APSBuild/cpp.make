# System wide defines for C++ compiler

SYSINCLUDES = $(BCB)\include;$(BCB)\include\vcl
SYSLIBPATH = $(BCB)\lib\obj;$(BCB)\lib

SYSCFLAGS = -O2 -H -H=$(APSROOT)\Bin\vcl60.csm  -Vx -Ve -X- -r- -a8 -b- -k- -vi- -c -tW 
SYSDEBUG =  -y -v 

