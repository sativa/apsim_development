#! 
# PricesUI. 
trace remove variable XMLDoc read setXML

package require Tk
package require BWidget
package require tdom

catch {destroy .w}
set w [frame .w]


proc getValue {id thing} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         return [$node text]
      }
   }
}

# Return a list of crops that are plugged into this simulation
proc findCrops {} {
   set knownCrops [list \
    barley       \
    bambatsi     \
    broccoli     \
    butterflypea \
    canola       \
    chickpea     \
    cowpea       \
    egrandis     \
    fababean     \
    fieldpea     \
    horsegram    \
    lablab       \
    lucerne      \
    lupin        \
    maize        \
    mucuna       \
    mungbean     \
    navybean     \
    oats         \
    ozcot        \
    peanut       \
    pigeonp      \
    rice         \
    sorghum      \
    soybean      \
    stylo        \
    sugar        \
    sunflower    \
    SweetCorn    \
    SweetSorghum \
    weed         \
    wheat]
   set result {}
   global GlobalXMLDoc
   set gdocroot [[dom parse $GlobalXMLDoc] documentElement]
   foreach crop $knownCrops {
      if {[llength [$gdocroot selectNodes //$crop]] > 0} {
          lappend result $crop
      }
   }
   return $result
}

foreach v {name desc price protein units updated} {
   catch {unset $v}
}

## Decode the XML string for this applet
set doc [dom parse $XMLDoc]
set docroot [$doc documentElement]

set category [[$docroot selectNodes //category] text]
set nodes {}
foreach node [$docroot selectNodes //$category] {
   set desc($node)    [getValue $node desc]
   set name($node)    [getValue $node name]
   set price($node)   [getValue $node price]
   set protein($node) [getValue $node protein]
   set units($node)   [getValue $node units]
   set updated($node) [getValue $node updated]
   lappend nodes $node
}

set row 1
label $w.t0 -text Description
label $w.t1 -text Price
label $w.t2 -text Units
label $w.t3 -text "Last Updated"
grid $w.t0 -row 0 -column 1 -padx 3
grid $w.t1 -row 0 -column 2 -padx 3
grid $w.t2 -row 0 -column 3 -padx 3
grid $w.t3 -row 0 -column 4 -padx 3

foreach node $nodes {
   label $w.l$row -text   $desc($node)
   if {$name($node) == "wheat"} {
      set f [frame $w.price$row]
      label $f.h1 -text "Protein (%)"
      label $f.h2 -text "Price (\$/tonne)"
      grid $f.h1 -row 1 -column 1; grid $f.h2 -row 1 -column 2
      set n [llength $protein($node)]; if {$n <= 4} {set n 4}
      for {set i 0} {$i < $n} {incr i} {
          set protein($node,$i) [lindex $protein($node) $i]
          set price($node,$i)   [lindex $price($node) $i]
          entry $f.pt$i -width 8 -textvariable protein($node,$i) -vcmd {string is int %P} 
          entry $f.pr$i -width 8 -textvariable price($node,$i) -vcmd {string is int %P} 
          grid $f.pt$i -row [expr $i+2] -column 1; grid $f.pr$i -row [expr $i+2] -column 2
      }
      label $w.u$row 
   } else {
      entry $w.price$row -width 8 -textvariable price($node) -vcmd {string is int %P} 
      label $w.u$row -text   $units($node)
   }   
   label $w.up$row -text  $updated($node)

   grid $w.l$row     -row $row -column 1 -pady 3 -sticky nw -padx 5
   grid $w.price$row -row $row -column 2 -pady 3 -sticky new
   grid $w.u$row     -row $row -column 3 -pady 3 -padx 5 -sticky nw
   grid $w.up$row    -row $row -column 4 -pady 3 -padx 5 -sticky nw
   # bind ... <3> ... postMenu $node
   incr row
}

grid rowconf    $w $row -weight 1
grid columnconf $w 5    -weight 1

grid $w -row 0 -column 0 -sticky nw
grid rowconf    . 0 -weight 1
grid columnconf . 0 -weight 1

# When the UI asks for XMLDoc, recreate the ascii form from the xml tree
trace add variable XMLDoc read setXML
proc setXML {name1 name2 op} {
   global XMLDoc doc docroot category nodes name price protein units updated
   catch {
      foreach node $nodes {
        set needsUpdate 0
        foreach child [$node childNodes] { 
           if {[$child nodeName] == "price"} {
              if {[info exists price($node,0)]} {
                 set price($node) {}
                 for {set i 0} {[info exists price($node,$i)]} {incr i} {
                    lappend price($node) $price($node,$i)
                 }
              }
              set old $child
              set new [$doc createElement "price"]
              $new appendChild [$doc createTextNode $price($node)]
              $node appendChild $new
              $old delete
              set needsUpdate 1
           } elseif {[$child nodeName] == "protein"} {
              if {[info exists protein($node,0)]} {
                 set protein($node) {}
                 for {set i 0} {[info exists protein($node,$i)]} {incr i} {
                    lappend protein($node) $protein($node,$i)
                 }
              }
              set old $child
              set new [$doc createElement "protein"]
              $new appendChild [$doc createTextNode $protein($node)]
              $node appendChild $new
              $old delete
              set needsUpdate 1
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
      global errorInfo; tk_messageBox -title "Error" -message "$msg:\n$errorInfo" -type ok
   } else {
     set XMLDoc [$doc asXML]
   }  
}
