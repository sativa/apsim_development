#! C:/development/apsim/FarmMachinery/CashBookUI.tcl
foreach c [winfo children .] {destroy $c}
trace remove variable XMLDoc read setXML

set w [frame .w]

package require tdom

## Decode the XML string for this applet
set doc [dom parse $XMLDoc]
set docroot [$doc documentElement]

set balance [[$docroot selectNodes //balance] text]
set outputfilename [[$docroot selectNodes //outputfilename] text]

label $w.of  -text "Filename"
entry $w.ofe -width 30 -textvariable outputfilename
button $w.ofb -text Browse -command chooseOutputFilename

label $w.ob  -text "Opening Balance (\$)"
entry $w.obe -width 12 -justify right -validate key -vcmd {string is int %P} -textvariable balance

grid $w.of  -row 1 -column 1 -sticky w   -padx 5 -pady 15
grid $w.ofe -row 1 -column 2 -sticky ew  -padx 5
grid $w.ofb -row 1 -column 3             -padx 5

grid $w.ob  -row 2 -column 1 -sticky w     -padx 5 -pady 5
grid $w.obe -row 2 -column 2 -sticky w -columnspan 2 -padx 5

grid rowconf    $w 3 -weight 1
grid columnconf $w 5 -weight 1

proc chooseOutputFilename {} {
   global outputfilename
   set types {{{Comma Separated Variable Files}   {.csv}}
              {{All Files}                        *}}
   set filename [tk_getSaveFile -filetypes $types -initialfile $outputfilename]
   if {$filename != ""} {
      set outputfilename $filename
   }
}

trace add variable XMLDoc read setXML
proc setXML {name1 name2 op} {
   global XMLDoc doc docroot balance outputfilename
   catch {
     foreach var {balance outputfilename} {
        set new [$doc createElement $var]
        $new appendChild [$doc createTextNode [set $var]]
        set old [$docroot selectNodes //$var] 
        [$old parentNode] appendChild $new
        $old delete
     }
   } msg
   if {$msg != ""} {tk_messageBox -title "Error" -message $msg -type ok}      
   set XMLDoc [$doc asXML]
}

grid $w -row 0 -column 0 -sticky nw
grid rowconf    . 0 -weight 1
grid columnconf . 0 -weight 1
