set apsuite c:/development
set fp [open "C:/Documents and Settings/devoilp/My Documents/zzz.apsim" r]
set GlobalXMLDoc [read $fp]

bind . <Escape> {+exec wish $argv0 &; exit}         ;# quick restart

close $fp
set XMLDoc {<tclui name="Generic Sowing Rule">
<uiscript>source $apsuite/apsim/economics/lib/PricesUI.tcl</uiscript>
<category>cropprice</category>
      <cropprice>
        <desc>Wheat</desc>
        <name>wheat</name>
        <units>$/tonne</units>
        <price>145.0 160.0 170.0 180.0 185.0</price>
        <protein>9.5  10.0  12.0  13.0  14.0</protein>
        <updated>26/10/2006</updated>
      </cropprice>
      <cropprice>
        <desc>Sorghum</desc>
        <name>sorghum</name>
        <price>120.0</price>
        <units>$/tonne</units>
        <updated />
      </cropprice>
</tclui>}

source  "C:/development/apsim/economics/lib/PricesUI.tcl"


button .test -text "Test" -command "setXML junk junk junk" 
grid .test 



