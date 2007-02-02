#! 
# TreatmentsUI. 
trace remove variable XMLDoc read setXML

package require Tk
package require BWidget
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
set category [[$docroot selectNodes //category] text]

# Build the UI
catch {destroy .w}
#set sw [ScrolledWindow .w]
#set sf [ScrollableFrame $sw.f]
#$sw setwidget $sf
#set w [$sf getframe]
set w [frame .w]

foreach v {desc name price units updated nodes} {
   catch {unset $v}
}

set nodes {}
foreach node [$docroot selectNodes //$category] {
   set desc($node)  [getValue $node desc]
   set name($node)  [getValue $node name]
   set price($node) [getValue $node price]
   set units($node) [getValue $node units]
   set updated($node) [getValue $node updated]
   lappend nodes $node
}

label $w.cat -text "Category $category"
grid $w.cat -row 0 -column 1 -padx 3  -sticky w

label $w.t0 -text Description
label $w.t1 -text "Apsim Name"
label $w.t2 -text Price
label $w.t3 -text Units
label $w.t4 -text "Last Updated"
grid $w.t0 -row 1 -column 1 -padx 3
grid $w.t1 -row 1 -column 2 -padx 3
grid $w.t2 -row 1 -column 3 -padx 3
grid $w.t3 -row 1 -column 4 -padx 3
grid $w.t4 -row 1 -column 5 -padx 3

set row 2
foreach node $nodes {
   label $w.l$row -text   $desc($node)
   entry $w.n$row -width 15 -textvariable name($node)
   entry $w.e$row -width 8 -textvariable price($node) -vcmd {string is int %P} 
   label $w.u$row -text   $units($node)
   label $w.up$row -text  $updated($node)

   grid $w.l$row -row $row -column 1 -sticky w -padx 5
   grid $w.n$row -row $row -column 2 -sticky w -padx 5
   grid $w.e$row -row $row -column 3 -sticky ew
   grid $w.u$row -row $row -column 4 -padx 5 -sticky w
   grid $w.up$row -row $row -column 5 -padx 5 -sticky w
# bind ... <3> ... postMenu $node
   incr row
}

grid rowconf    $w $row -weight 1
grid columnconf $w 5    -weight 1

grid .w -row 0 -column 0 -sticky nesw
grid rowconf    . 0 -weight 1
grid columnconf . 0 -weight 1

# When the UI asks for XMLDoc, recreate the ascii form from the xml tree
trace add variable XMLDoc read setXML
proc setXML {name1 name2 op} {
   global XMLDoc doc docroot category nodes name price units updated
   catch {
      foreach node $nodes {
        set needsUpdate 0
        foreach child [$node childNodes] { 
           if {[$child nodeName] == "price"} {
              if {$price($node) != [$child text]} {
                 set old $child
                 set new [$doc createElement "price"]
                 $new appendChild [$doc createTextNode $price($node)]
                 $node appendChild $new
                 $old delete
                 set needsUpdate 1
              }
           } elseif {[$child nodeName] == "name"} {
              if {$name($node) != [$child text]} {
                 set old $child
                 set new [$doc createElement "name"]
                 $new appendChild [$doc createTextNode $name($node)]
                 $node appendChild $new
                 $old delete
              }
           }      
         }
         if {$needsUpdate} {
            set old {}
            foreach child [$node childNodes] { 
               if {[$child nodeName] == "updated"} {
                   set old $child
               }
            }   
            set new [$doc createElement "updated"]
            $new appendChild [$doc createTextNode [clock format [clock seconds] -format "%d/%m/%Y"]]
            $node appendChild $new
            if {$old != {}} {$old delete}
         }
      }     
   } msg
   if {$msg != ""} {
     tk_messageBox -title "Error" -message $msg -type ok
   } else {
     set XMLDoc [$doc asXML]
   }  
}

