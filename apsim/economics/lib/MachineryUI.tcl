#! C:/development/apsim/FarmMachinery/MachineryUI.tcl
foreach c [winfo children .] {destroy $c}
trace remove variable XMLDoc read setXML

foreach what {MarketValue SalvageValue YearsToReplacement age name replacementMethod} {
   catch {unset $what}
}

package require tdom
proc getValue {id thing} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         return [$node text]
      }
   }
}

## Decode the XML string for this applet
set doc [dom parse $XMLDoc]
set docroot [$doc documentElement]

# We will also need a list of all implements used in the simulation
set gdocroot [[dom parse $GlobalXMLDoc] documentElement]

set node [$docroot selectNodes //category]
if {$node == {}} {tk_messageBox -title "Error" -message "Missing category in XML" -type ok; return}
set category [$node text]

set catroot [$docroot selectNodes //$category]
if {$catroot == {}} {tk_messageBox -title "Error" -message "Missing $category in XML" -type ok; return}

foreach what {MarketValue SalvageValue YearsToReplacement age name replacementMethod} {
   set $what [getValue $catroot $what]
}

set w [frame .w]
label $w.img  -image [image create photo -data [getValue $catroot image] -format gif]
label $w.nameLabel                -text "Apsim Name"
entry $w.name                     -textvariable name 

label $w.marketValueLabel         -text "Market Value ($)"
entry $w.marketValue              -textvariable MarketValue -width 8 -vcmd {string is int %P} 
label $w.salvageValueLabel        -text "Salvage Value ($)"
entry $w.salvageValue             -textvariable SalvageValue -width 8  -vcmd {string is int %P} 
label $w.yearsToReplacementLabel  -text "Useful Life (y)"
entry $w.yearsToReplacement       -textvariable YearsToReplacement -width 4 -vcmd {string is int %P} 
label $w.ageLabel                 -text "Initial Age (y)"
entry $w.age                      -textvariable age -width 4  -vcmd {string is int %P} 

grid $w.nameLabel         -row 1 -column 1 -sticky w     -pady 3  
grid $w.name              -row 1 -column 2 -sticky w     -pady 3  
grid $w.img               -row 1 -column 3 -sticky n     -pady 3  -rowspan 6 
grid $w.marketValueLabel  -row 2 -column 1 -sticky w     -pady 3 
grid $w.marketValue       -row 2 -column 2 -sticky w
grid $w.salvageValueLabel -row 3 -column 1 -sticky w     -pady 3 
grid $w.salvageValue      -row 3 -column 2 -sticky w
grid $w.yearsToReplacementLabel  -row 4 -column 1 -sticky w -pady 3 
grid $w.yearsToReplacement       -row 4 -column 2 -sticky w
grid $w.ageLabel  -row 5 -column 1 -sticky w
grid $w.age       -row 5 -column 2 -sticky w             -pady 3 

grid columnconf $w 3 -weight 1
grid rowconf    $w 6 -weight 1

catch { unset fuelrate fuelnode workrate worknode }
if {$category == "tractor"} {
   frame $w.f
   label $w.f.in -text "Implement"
   label $w.f.fr -text "Fuel Rate (lts/hour)"
   label $w.f.wr -text "Work Rate (ha/hour)"
   grid $w.f.in -row 1 -column 1  -pady 3 -padx 5
   grid $w.f.fr -row 1 -column 2  -pady 3 -padx 5 
   grid $w.f.wr -row 1 -column 3  -pady 3 -padx 5

   set row 2
   set implements {}

   # Go through all implements in the current simulation and find a fuel/work rate for them.
   foreach node [$gdocroot selectNodes //implement] {
      set implementName [getValue $node name]
      label $w.f.in$row -text $implementName
      set fuelrate($node) {}
      foreach tnode [$catroot childNodes] {
         if {[string equal -nocase [$tnode nodeName] "fuelrate"] && 
             [string equal -nocase [$tnode getAttribute implement] $implementName]} {
            set fuelrate($node) [$tnode text]
         }  
      }
      entry $w.f.fr$row -textvariable fuelrate($node) -width 6

      set workrate($node) {}
      foreach tnode [$catroot childNodes] {
         if {[string equal -nocase [$tnode nodeName] "workrate"] && 
             [string equal -nocase [$tnode getAttribute implement] $implementName]} {
            set workrate($node) [$tnode text]
         }  
      }
      entry $w.f.wr$row -textvariable workrate($node) -width 6
   
      lappend implements $node
      grid $w.f.in$row -row $row -column 1 -sticky w -pady 3 
      grid $w.f.fr$row -row $row -column 2 -sticky w -pady 3 
      grid $w.f.wr$row -row $row -column 3 -sticky w -pady 3 
      incr row
   }
   grid $w.f -row 7 -column 1 -sticky nw -columnspan 4
}

grid rowconf $w 100 -weight 1

proc setXML {name1 name2 op} {
   global XMLDoc doc docroot catroot category
   catch {
     foreach var {MarketValue SalvageValue YearsToReplacement age name} {
        global $var
        set new [$doc createElement $var]
        $new appendChild [$doc createTextNode [set $var]]
        foreach tnode [$catroot childNodes] {
           if {[string equal -nocase [$tnode nodeName] $var]} {
              [$tnode parentNode] appendChild $new
              $tnode delete
           }
        }
     }
     if {$category == "tractor"} {
        global worknode workrate fuelrate implements
        foreach node $implements {
           set implementName [getValue $node name]
           set new [$doc createElement workrate]
           $new setAttribute implement $implementName
           $new appendChild [$doc createTextNode $workrate($node)]
           foreach tnode [$catroot childNodes] {
              if {[string equal -nocase [$tnode nodeName] workrate] &&
                  [string equal -nocase [$tnode getAttribute implement] $implementName]} {
                 $tnode delete
              }
           }   
           $catroot appendChild $new

           set new [$doc createElement fuelrate]
           $new setAttribute implement $implementName
           $new appendChild [$doc createTextNode $fuelrate($node)]
           foreach tnode [$catroot childNodes] {
              if {[string equal -nocase [$tnode nodeName] fuelrate] &&
                  [string equal -nocase [$tnode getAttribute implement] $implementName]} {
                 $tnode delete
              }
           }   
           $catroot appendChild $new
        }
     }
   } msg
   if {$msg != ""} {
      global errorInfo; tk_messageBox -title "Error" -message "$msg:\n$errorInfo" -type ok
   } else {
      set XMLDoc [$doc asXML]
   }
}
trace add variable XMLDoc read setXML

grid $w -row 0 -column 0 -sticky nwse
grid rowconf    . 0 -weight 1
grid columnconf . 0 -weight 1

##setXML a b read