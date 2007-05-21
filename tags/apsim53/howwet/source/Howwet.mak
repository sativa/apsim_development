#
# Borland C++ IDE generated makefile
# Generated 10/11/2004 at 12:38:12 PM 
#
.AUTODEPEND


#
# Borland C++ tools
#
IMPLIB  = Implib
BCC32   = Bcc32 +BccW32.cfg 
BCC32I  = Bcc32i +BccW32.cfg 
TLINK32 = TLink32
TLIB    = TLib
BRC32   = Brc32
TASM32  = Tasm32
#
# IDE macros
#


#
# Options
#
IDE_LinkFLAGS32 =  -LC:\BC5\LIB
IDE_ResFLAGS32 = 
LinkerW32Debug_Info_lNoner = 
ResW32Debug_Info_lNoner = 
BW32Debug_Info_lNoner = 
LinkerStyleSheetAtW32_howwetdexe = $(LinkerW32Debug_Info_lNoner)
ResStyleSheetAtW32_howwetdexe = $(ResW32Debug_Info_lNoner)
BStyleSheetAtW32_howwetdexe = $(BW32Debug_Info_lNoner)
LinkerLocalOptsAtW32_howwetdexe =  -LC:\BC5\LIB;..\SHARED -Tpe -aa -V4.0 -c
ResLocalOptsAtW32_howwetdexe = 
BLocalOptsAtW32_howwetdexe = 
CompInheritOptsAt_howwetdexe = -I.\;C:\BC5\INCLUDE;..\SHARED -DSTRICT;_OWLPCH;
LinkerInheritOptsAt_howwetdexe = -x
LinkerOptsAt_howwetdexe = $(LinkerStyleSheetAtW32_howwetdexe) $(LinkerLocalOptsAtW32_howwetdexe)
ResOptsAt_howwetdexe = $(ResStyleSheetAtW32_howwetdexe) $(ResLocalOptsAtW32_howwetdexe)
BOptsAt_howwetdexe = $(BStyleSheetAtW32_howwetdexe) $(BLocalOptsAtW32_howwetdexe)

#
# Dependency List
#
Dep_Howwet = \
   howwet.exe

Howwet : BccW32.cfg $(Dep_Howwet)
  echo MakeNode

Dep_howwetdexe = \
   rectdrag.obj\
   dragpts.obj\
   calender.obj\
   texttble.obj\
   markers.obj\
   iseries.obj\
   ilegend.obj\
   penwidts.obj\
   brshstys.obj\
   penstyls.obj\
   colours.obj\
   iaxis.obj\
   iggdi.obj\
   ibase.obj\
   iobject.obj\
   sersetup.obj\
   dchart2.obj\
   interact.obj\
   ggdi.obj\
   dobject.obj\
   dbase.obj\
   globals.obj\
   daxis.obj\
   dlegend.obj\
   dseries.obj\
   gpoint.obj\
   printout.obj\
   dchart.obj\
   objlist.obj\
   intarray.obj\
   gdatecol.obj\
   ginifile.obj\
   brkupstr.obj\
   gpath.obj\
   srchstrm.obj\
   gstring.obj\
   gdate.obj\
   grealcol.obj\
   gcharcol.obj\
   gcolumn.obj\
   gcolumns.obj\
   ownerdr.obj\
   cldrcont.obj\
   viewbox.obj\
   anim_dlg.obj\
   chartcon.obj\
   rainfall\rainfall.lib\
   lineobj.obj\
   eros_dlg.obj\
   shadeobj.obj\
   how_anim.obj\
   prof_win.obj\
   watn_dlg.obj\
   rain_avg.obj\
   sw_graph.obj\
   so_what.obj\
   nitgraph.obj\
   simul.obj\
   cal_dlg.obj\
   rainanal.obj\
   res_dlg.obj\
   clmt_dlg.obj\
   scon_dlg.obj\
   soil_dlg.obj\
   params.obj\
   howwet.obj\
   how_main.obj\
   howwet.res\
   howwtapp.def

howwet.exe : $(Dep_howwetdexe)
  $(TLINK32) @&&|
 /v $(IDE_LinkFLAGS32) $(LinkerOptsAt_howwetdexe) $(LinkerInheritOptsAt_howwetdexe) +
C:\BC5\LIB\c0w32.obj+
rectdrag.obj+
dragpts.obj+
calender.obj+
texttble.obj+
markers.obj+
iseries.obj+
ilegend.obj+
penwidts.obj+
brshstys.obj+
penstyls.obj+
colours.obj+
iaxis.obj+
iggdi.obj+
ibase.obj+
iobject.obj+
sersetup.obj+
dchart2.obj+
interact.obj+
ggdi.obj+
dobject.obj+
dbase.obj+
globals.obj+
daxis.obj+
dlegend.obj+
dseries.obj+
gpoint.obj+
printout.obj+
dchart.obj+
objlist.obj+
intarray.obj+
gdatecol.obj+
ginifile.obj+
brkupstr.obj+
gpath.obj+
srchstrm.obj+
gstring.obj+
gdate.obj+
grealcol.obj+
gcharcol.obj+
gcolumn.obj+
gcolumns.obj+
ownerdr.obj+
cldrcont.obj+
viewbox.obj+
anim_dlg.obj+
chartcon.obj+
lineobj.obj+
eros_dlg.obj+
shadeobj.obj+
how_anim.obj+
prof_win.obj+
watn_dlg.obj+
rain_avg.obj+
sw_graph.obj+
so_what.obj+
nitgraph.obj+
simul.obj+
cal_dlg.obj+
rainanal.obj+
res_dlg.obj+
clmt_dlg.obj+
scon_dlg.obj+
soil_dlg.obj+
params.obj+
howwet.obj+
how_main.obj
$<,$*
rainfall\rainfall.lib+
C:\BC5\LIB\owlwf.lib+
C:\BC5\LIB\bidsf.lib+
C:\BC5\LIB\import32.lib+
C:\BC5\LIB\cw32.lib
howwtapp.def
howwet.res

|
rectdrag.obj :  ..\shared\gobjsx\rectdrag.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjsx\rectdrag.cpp
|

dragpts.obj :  ..\shared\gobjsx\dragpts.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjsx\dragpts.cpp
|

calender.obj :  ..\shared\gobjsx\calender.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjsx\calender.cpp
|

texttble.obj :  ..\shared\gobjsx\texttble.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjsx\texttble.cpp
|

markers.obj :  ..\shared\graph\markers.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\markers.cpp
|

iseries.obj :  ..\shared\graph\iseries.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\iseries.cpp
|

ilegend.obj :  ..\shared\graph\ilegend.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\ilegend.cpp
|

penwidts.obj :  ..\shared\graph\penwidts.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\penwidts.cpp
|

brshstys.obj :  ..\shared\graph\brshstys.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\brshstys.cpp
|

penstyls.obj :  ..\shared\graph\penstyls.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\penstyls.cpp
|

colours.obj :  ..\shared\graph\colours.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\colours.cpp
|

iaxis.obj :  ..\shared\graph\iaxis.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\iaxis.cpp
|

iggdi.obj :  ..\shared\graph\iggdi.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\iggdi.cpp
|

ibase.obj :  ..\shared\graph\ibase.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\ibase.cpp
|

iobject.obj :  ..\shared\graph\iobject.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\iobject.cpp
|

sersetup.obj :  ..\shared\graph\sersetup.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\sersetup.cpp
|

dchart2.obj :  ..\shared\graph\dchart2.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\dchart2.cpp
|

interact.obj :  ..\shared\graph\interact.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\interact.cpp
|

ggdi.obj :  ..\shared\graph\ggdi.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\ggdi.cpp
|

dobject.obj :  ..\shared\graph\dobject.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\dobject.cpp
|

dbase.obj :  ..\shared\graph\dbase.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\dbase.cpp
|

globals.obj :  ..\shared\graph\globals.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\globals.cpp
|

daxis.obj :  ..\shared\graph\daxis.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\daxis.cpp
|

dlegend.obj :  ..\shared\graph\dlegend.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\dlegend.cpp
|

dseries.obj :  ..\shared\graph\dseries.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\dseries.cpp
|

gpoint.obj :  ..\shared\graph\gpoint.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\gpoint.cpp
|

printout.obj :  ..\shared\graph\printout.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\printout.cpp
|

dchart.obj :  ..\shared\graph\dchart.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\dchart.cpp
|

objlist.obj :  ..\shared\graph\objlist.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\graph\objlist.cpp
|

intarray.obj :  ..\shared\gobjs\intarray.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\intarray.cpp
|

gdatecol.obj :  ..\shared\gobjs\gdatecol.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\gdatecol.cpp
|

ginifile.obj :  ..\shared\gobjs\ginifile.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\ginifile.cpp
|

brkupstr.obj :  ..\shared\gobjs\brkupstr.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\brkupstr.cpp
|

gpath.obj :  ..\shared\gobjs\gpath.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\gpath.cpp
|

srchstrm.obj :  ..\shared\gobjs\srchstrm.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\srchstrm.cpp
|

gstring.obj :  ..\shared\gobjs\gstring.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\gstring.cpp
|

gdate.obj :  ..\shared\gobjs\gdate.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\gdate.cpp
|

grealcol.obj :  ..\shared\gobjs\grealcol.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\grealcol.cpp
|

gcharcol.obj :  ..\shared\gobjs\gcharcol.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\gcharcol.cpp
|

gcolumn.obj :  ..\shared\gobjs\gcolumn.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\gcolumn.cpp
|

gcolumns.obj :  ..\shared\gobjs\gcolumns.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\gobjs\gcolumns.cpp
|

ownerdr.obj :  ..\shared\cl\ownerdr.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\cl\ownerdr.cpp
|

cldrcont.obj :  ..\shared\cl\cldrcont.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\cl\cldrcont.cpp
|

viewbox.obj :  ..\shared\cl\viewbox.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\cl\viewbox.cpp
|

anim_dlg.obj :  ..\shared\cl\anim_dlg.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\cl\anim_dlg.cpp
|

chartcon.obj :  ..\shared\cl\chartcon.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ ..\shared\cl\chartcon.cpp
|

lineobj.obj :  lineobj.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ lineobj.cpp
|

eros_dlg.obj :  eros_dlg.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ eros_dlg.cpp
|

shadeobj.obj :  shadeobj.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ shadeobj.cpp
|

how_anim.obj :  how_anim.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ how_anim.cpp
|

prof_win.obj :  prof_win.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ prof_win.cpp
|

watn_dlg.obj :  watn_dlg.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ watn_dlg.cpp
|

rain_avg.obj :  rain_avg.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ rain_avg.cpp
|

sw_graph.obj :  sw_graph.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ sw_graph.cpp
|

so_what.obj :  so_what.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ so_what.cpp
|

nitgraph.obj :  nitgraph.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ nitgraph.cpp
|

simul.obj :  simul.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ simul.cpp
|

cal_dlg.obj :  cal_dlg.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ cal_dlg.cpp
|

rainanal.obj :  rainanal.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ rainanal.cpp
|

res_dlg.obj :  res_dlg.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ res_dlg.cpp
|

clmt_dlg.obj :  clmt_dlg.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ clmt_dlg.cpp
|

scon_dlg.obj :  scon_dlg.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ scon_dlg.cpp
|

soil_dlg.obj :  soil_dlg.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ soil_dlg.cpp
|

params.obj :  params.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ params.cpp
|

howwet.obj :  howwet.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ howwet.cpp
|

how_main.obj :  how_main.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe) -o$@ how_main.cpp
|

howwet.res :  howwet.rc
  $(BRC32) -R @&&|
 $(IDE_ResFLAGS32) $(ROptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe)  -FO$@ howwet.rc
|
printer.res :  ..\..\..\..\bc5\include\owl\printer.rc
  $(BRC32) -R @&&|
 $(IDE_ResFLAGS32) $(ROptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe)  -FO$@ ..\..\..\..\bc5\include\owl\printer.rc
|
slider.res :  ..\..\..\..\bc5\include\owl\slider.rc
  $(BRC32) -R @&&|
 $(IDE_ResFLAGS32) $(ROptsAt_howwetdexe) $(CompInheritOptsAt_howwetdexe)  -FO$@ ..\..\..\..\bc5\include\owl\slider.rc
|
# Compiler configuration file
BccW32.cfg : 
   Copy &&|
-w
-R
-v
-vi
-H
-H=Howwet.csm
-v-
-vi
-R-
-k-
-W
-H"owl\pch.h"
| $@


