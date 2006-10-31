#! C:/development/apsim/Economics/OverheadsUI.tcl

foreach c [winfo children .] {destroy $c}
trace remove variable XMLDoc read setXML

foreach v {name desc value} {
  if {[info exists $v]} {unset $v}
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

set node [$docroot selectNodes //category]
if {$node == {}} {tk_messageBox -title "Error" -message "Missing category in XML" -type ok; return}
set category [$node text]

set w [frame .w]

label $w.title -text "Annual Farm Overhead expenses"
grid $w.title  -row 1 -column 1 -sticky w  -padx 5 -columnspan 3

set row 2

set names {}
foreach node [$docroot selectNodes //$category] {
  set name [getValue $node name]
  set desc [getValue $node description]
  set value [getValue $node value]

  set $name $value
  
  label $w.l$name -text $desc
  entry $w.e$name -textvariable $name -width 10 -vcmd {string is int %P} -justify right

  grid $w.l$name  -row $row -column 1 -sticky w  -padx 5
  grid $w.e$name  -row $row -column 2 -sticky ew -padx 5
  incr row
  lappend names $name
  trace add variable $name write updateSum
}

label $w.tsum -text $desc -text "Total"
label $w.sum -text $desc -textvariable sum
grid  $w.tsum -row $row -column 1 -sticky e -padx 5 -sticky e
grid  $w.sum -row $row -column 2 -sticky ew -padx 5 -sticky e
incr row

proc updateSum {a b c} {
   global names sum
   set sum 0.0
   foreach name $names {
      global $name
      set sum [expr $sum + [set $name]]
   }
   set sum "\$$sum"
}

updateSum junk junk junk

grid columnconf $w 3 -weight 1
grid rowconf    $w $row -weight 1

proc setXML {name1 name2 op} {
   global XMLDoc doc docroot names category
   catch {
      foreach name $names {
         global $name
         set new [$doc createElement value]
         $new appendChild [$doc createTextNode [set $name]]
         foreach node [$docroot selectNodes //$category] {
            if {$name == [getValue $node name]} {
               foreach tnode [$node childNodes] {
                  if {[string equal -nocase [$tnode nodeName] value]} {
                     $tnode delete
                  }   
               }
               $node appendChild $new
            }
         }
         trace remove variable $name write updateSum
      }
   } msg
   if {$msg != ""} {global errorInfo; tk_messageBox -title "Error" -message "$msg:\n$errorInfo" -type ok}
   set XMLDoc [$doc asXML]
}
trace add variable XMLDoc read setXML

grid $w -row 0 -column 0 -sticky nwse
grid rowconf    . 0 -weight 1
grid columnconf . 0 -weight 1

##setXML a b read