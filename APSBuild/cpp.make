# System wide defines for C++ compiler
DEBUG=

SYSDEFINES = NO_STRICT;_NO_VCL;_RTLDLL;
SYSINCLUDES = $(BCB)\include;$(APSROOT)\Shared;$(BCB)\Components\Boost
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
SYSCFLAGS = $(SYSCFLAGS) -O2 -w-inl
SYSLDFLAGS = $(SYSLDFLAGS)
SYSLIBS = 
!else
SYSCFLAGS = $(SYSCFLAGS) -Od -v -y -R -w-inl -vG
SYSLDFLAGS = $(SYSLDFLAGS) -v 
SYSLIBS = cg32.lib
!endif

# CodeGuard:
## -vG, cg32.lib 

CC=$(BCB)\Bin\bcc32.exe
LD=$(BCB)\Bin\ilink32

