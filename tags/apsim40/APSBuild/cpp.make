# System wide defines for C++ compiler

SYSINCLUDES = $(BCB)\include;$(BCB)\include\vcl;$(BCB)\Components\Boost
SYSLIBPATH = $(BCB)\lib\obj;$(BCB)\lib
SYSCFLAGS = -H=$(APSROOT)\Bin\vcl60.csm -Hc -Vx -Ve -X- -a8 -b- -tWM -c
#componentinterface = -O2 -vi -x- -RT- -TW
#general            = -Od -vi- -TW
#apsimshared        = -O2 -vi -TWD
#protocol           = -O2 -vi -TWD
#protocolmanager    = -O2 -vi -TWD
#modules            = -O2 -vi -TWD
SYSLDFLAGS = 

# Optimisation and debug symbols.
!if "$(DEBUG)" == ""
SYSCFLAGS = $(SYSCFLAGS) -O2
SYSLDFLAGS = $(SYSLDFLAGS)
!else
SYSCFLAGS = $(SYSCFLAGS) -Od -v -y
SYSLDFLAGS = $(SYSLDFLAGS) -v
!endif

# These are harvested from dean's individual bprs:
#apsimshared
#       -WD -O2 -H=$(BCB)\lib\vcl60.csm -Hc -Vx -Ve -X- -a8 -b- -k- -vi -tWD -tWM -c
#componentinterface
#           -O2 -H=$(BCB)\lib\vcl60.csm -Hc -Vx -Ve -X- -a8 -b- -k- -vi  -tW -tWM -c -x- -RT- 
#general
#           -Od -H=$(BCB)\lib\vcl60.csm -Hc -Vx -Ve -X- -a8 -b- -k  -vi- -tW -tWM -c -r- -y -v 
#protocol
#       -WD -O2 -H=$(BCB)\lib\vcl60.csm -Hc -Vx -Ve -X- -a8 -b- -k- -vi -tWD -tWM -c
#protocolmanager
#       -WD -O2 -H=$(BCB)\lib\vcl60.csm -Hc -Vx -Ve -X- -a8 -b- -k- -vi -tWD -tWM -c    
#modules
#       -WD -O2 -H=$(BCB)\lib\vcl60.csm -Hc -Vx -Ve -X- -a8 -b- -k- -vi -c -tWM

