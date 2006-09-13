#! 
# PricesUI. 
foreach c [winfo children .] {destroy $c}
trace remove variable XMLDoc read setXML

foreach v {name desc price units updated} {
  if {[info exists $v]} {unset $v}
}

##set fp [open tst.xml r]; set XMLDoc [read $fp]; close $fp

package require Tk
set w [frame .w]

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
set nodes {}
foreach node [$docroot selectNodes //$category] {
   set desc($node)  [getValue $node desc]
   set name($node)  [getValue $node name]
   set price($node) [getValue $node price]
   set units($node) [getValue $node units]
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

proc textvar_tie {w name1 name2 op} {
    upvar #0 $name1 var
    switch $op {
        read  {set var($name2) [$w get 1.0 end-1c]}
        write {$w delete 1.0 end; $w insert end $var($name2)}
    }
}

foreach node $nodes {
   label $w.l$row -text   $desc($node)
   # Deal with multiline entries via a linked text widget
   set nlines [expr 1 + [regexp -all {\n} $name($node) {} junk]]
   if {$nlines > 1} {
      text  $w.price$row -width 30 -height $nlines 
      $w.price$row insert end $price($node)
      trace add variable price($node) {read write}  "textvar_tie $w.price$row"
   } else {
      entry $w.price$row -width 8 -textvariable price($node) -vcmd {string is int %P} 
   }   
   label $w.u$row -text   $units($node)
   label $w.up$row -text  $updated($node)

   grid $w.l$row -row $row -column 1 -sticky nw -padx 5
   grid $w.price$row -row $row -column 2 -sticky new
   grid $w.u$row -row $row -column 3 -padx 5 -sticky nw
   grid $w.up$row -row $row -column 4 -padx 5 -sticky nw
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

##puts "$XMLDoc"