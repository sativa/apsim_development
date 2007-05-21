set apsuite c:/development
set fp [open "C:/Documents and Settings/devoilp/My Documents/zzz.apsim" r]
set GlobalXMLDoc [read $fp]

bind . <Escape> {+exec wish $argv0 &; exit}         ;# quick restart

close $fp
set XMLDoc {<tclui name="Generic Sowing Rule">
<uiscript>source $apsuite/apsim/manager/lib/RotationRugPlotUI.tcl</uiscript>
<filename>C:\Documents and Settings\devoilp\My Documents\Projects\ApsFarm\Rotation Sample 2.log</filename>
</tclui>}

source  "C:/development/apsim/manager/lib/RotationRugPlotUI.tcl"

