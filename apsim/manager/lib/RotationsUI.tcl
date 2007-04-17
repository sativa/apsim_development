package req Tk
package req struct
package req tdom
trace remove variable XMLDoc read setXML

# Get the value of one of our parameters
proc getValue {id thing} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         return [$node text]
      }
   }
   return ""
}

# Get the values of one of our parameters
proc getValues {id thing} {
   set result {}
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         lappend result [$node text]
      }
   }
   return $result
}

# return the tag of an item near (x,y)
proc getTag {c x y} {
   set tag [lindex [$c find overlapping [expr $x-5] [expr $y-5] [expr $x+5] [expr $y+5]] end]
   if {[llength $tag] > 0} {
      foreach tag [$c gettags $tag] {
         if {$tag != "current"} {
             return $tag
         }
      }
   }
   return {}  
}

# Select an object near (x,y)
proc select {w x y} {
   upvar #0 $w cfg
   if {$cfg(selected) != {}} {
      foreach {type name} [split $cfg(selected) :] {break}
      if {$type == "node"} {
          catch {$cfg(c) itemconf $cfg(selected) -outline black -width 1}
      } elseif {$type == "arc"} {
          catch {$cfg(c) itemconf $cfg(selected) -fill black -width 1}
      }
   }
   set tag [getTag $cfg(c) $x $y]
   if {$tag != {}} {
      set cfg(selected) $tag
      set cfg(selectedX) $x
      set cfg(selectedY) $y
      foreach {type name} [split $tag :] {break}
      if {$type == "node"} {
        catch {$cfg(c) itemconf $tag -outline blue -width 3}
      } elseif {$type == "arc"} {
        catch {$cfg(c) itemconf $tag -fill blue -width 3}
      }
      showSelection $w $type $name
   } else {
      set cfg(selected) {}
      set cfg(selectedX) {}
      set cfg(selectedY) {}
      set cfg(message) "No items selected."
      showSelection $w help dummy
   }
}

# Move an already selected object 
proc move {w x y} {
   upvar #0 $w cfg
   if {$cfg(selected) != {}} {
      set dx [expr {$x - $cfg(selectedX)}]
      set dy [expr {$y - $cfg(selectedY)}]
      set cfg(selectedX) $x
      set cfg(selectedY) $y
      foreach {type name} [split $cfg(selected) :] {break}
      if {$type == "node"} {
         $cfg(c) move $cfg(selected) $dx $dy
         foreach {x1 y1 x2 y2} [$cfg(c) coords $cfg(selected)] {}
         foreach what {x1 y1 x2 y2} {$cfg(g) node set $name $what [set $what]}
         foreach arc [$cfg(g) arcs -adj $name] {
            fixupArrow $w arc:$arc "node:[$cfg(g) arc source $arc]" "node:[$cfg(g) arc target $arc]"
         }
      } elseif {$type == "arc"} {
         set coords [$cfg(c) coords $cfg(selected)]
         set newx [expr $dx + [lindex $coords 2]]
         set newy [expr $dy + [lindex $coords 3]]
         $cfg(g) arc set $name x $newx
         $cfg(g) arc set $name y $newy
         fixupArrow $w $cfg(selected) "node:[$cfg(g) arc source $name]" "node:[$cfg(g) arc target $name]"
      }  
   }
   $cfg(c) configure -scrollregion [$cfg(c) bbox all]
}

 
#Return a point on the edge of the node on a line to (x,y)
# NB. canvas origin at upper left corner
# Used to find arrow positions between nodes
proc findEdge {w x y node} {
   upvar #0 $w cfg
   set maxy [$w cget -height]

   # center of node
   set nc [$cfg(c) coords $node]
   set xn [expr ([lindex $nc 2] + [lindex $nc 0])/2.0]
   set yn [expr $maxy - ([lindex $nc 3] + [lindex $nc 1])/2.0]

   # angle of midline
   set a 0.0
   catch {set a [expr atan((($maxy-$y)-$yn)/($x-$xn))]}

   # Cheat - assume its always a circle and just take mid point of box
   set r [expr abs(([lindex $nc 2] - [lindex $nc 0])/2.0)]
   set dx [expr $x-$xn]
   set dy [expr ($maxy-$y)-$yn]                
   if {$dx < 0} {
     set xe [expr $xn - $r * cos($a)]
     set ye [expr $yn - $r * sin($a)] 
   } else { 
     set xe [expr $xn + $r * cos($a)]
     set ye [expr $yn + $r * sin($a)] 
   }  
   return [list $xe [expr $maxy - $ye]]
}

# Set the arc coordinates between 2 nodes.
proc fixupArrow {w arc srcNode destNode} {
   upvar #0 $w cfg
   set arc [lindex [split $arc ":"] 1]
   set xm [$cfg(g) arc get $arc x] 
   set ym [$cfg(g) arc get $arc y]

   set p0 [findEdge $w $xm $ym $srcNode]
   set p1 [findEdge $w $xm $ym $destNode]
   
   $cfg(c) coords arc:$arc [concat $p0 $xm $ym $p1]
}

# Right button click - post a context sensitive menu
proc action {w x y cx cy} {
   upvar #0 $w cfg
   set tag [getTag $cfg(c) $cx $cy]
   if {$cfg(selected) == {}} {
      postNewNodeMenu $w $x $y $cx $cy
   } elseif {$cfg(selected) != {} && "$tag" == $cfg(selected)} {
      postChangeMenu $w $tag $x $y $cx $cy 
   } elseif {$cfg(selected) != {} && "$tag" != {} && "$tag" != $cfg(selected)} {
      postAddArcMenu $w $cfg(selected) $tag $x $y $cx $cy 
   }
}

proc postNewNodeMenu {w x y cx cy} {
   upvar #0 $w cfg
   if {[winfo exists $cfg(c).newNodeMenu]} {destroy $cfg(c).newNodeMenu}
   menu $cfg(c).newNodeMenu -tearoff 0
   $cfg(c).newNodeMenu add command -label "Add New State" -command "addNewNode $w $cx $cy"
   tk_popup $cfg(c).newNodeMenu $x $y 
}

proc addNewNode {w x y} {
   upvar #0 $w cfg
   set n 0; while {[$cfg(g) node exists n$n]} {incr n}; set n "n$n"
   $cfg(g) node insert $n
   $cfg(g) node set $n x1 [set x1 [expr $x-50]]
   $cfg(g) node set $n y1 [set y1 [expr $y-50]]
   $cfg(g) node set $n x2 [set x2 [expr $x+50]]
   $cfg(g) node set $n y2 [set y2 [expr $y+50]]
   $cfg(g) node set $n fill brown
   $cfg(g) node set $n name $n
   $cfg(g) node set $n desc  "New state"
   $cfg(g) node set $n alias "newnode"

   $cfg(c) create oval $x1 $y1 $x2 $y2 -fill brown -tags node:$n
   set mx($n) [expr ($x1 + $x2) / 2.0]; set my($n) [expr ($y1 + $y2) / 2.0]
   $cfg(c) create text $mx($n) $my($n) -text "$n" -anchor center -tags node:$n

   select $w [$cfg(c) canvasx $x] [$cfg(c) canvasy $y]
   $cfg(c) configure -scrollregion [$cfg(c) bbox all]
}

proc dupNode {w tag} {
   upvar #0 $w cfg
   set srcNode [lindex [split $tag :] 1] 

   set n 0; while {[$cfg(g) node exists n$n]} {incr n}; set n "n$n"
   $cfg(g) node insert $n
   $cfg(g) node set $n x1 [set x1 [expr [$cfg(g) node get $srcNode x1]+50]]
   $cfg(g) node set $n y1 [set y1 [expr [$cfg(g) node get $srcNode y1]+50]]
   $cfg(g) node set $n x2 [set x2 [expr [$cfg(g) node get $srcNode x2]+50]]
   $cfg(g) node set $n y2 [set y2 [expr [$cfg(g) node get $srcNode y2]+50]]
   $cfg(g) node set $n fill [$cfg(g) node get $srcNode fill]
   $cfg(g) node set $n name [$cfg(g) node get $srcNode name]
   $cfg(g) node set $n desc [$cfg(g) node get $srcNode desc]
   $cfg(g) node set $n alias [$cfg(g) node get $srcNode alias]

   $cfg(c) create oval $x1 $y1 $x2 $y2 -fill [$cfg(g) node get $srcNode fill] -tags node:$n
   set mx($n) [expr ($x1 + $x2) / 2.0]; set my($n) [expr ($y1 + $y2) / 2.0]
   $cfg(c) create text $mx($n) $my($n) -text [$cfg(g) node get $srcNode name] -anchor center -tags node:$n

   foreach arc [$cfg(g) arcs -in $srcNode] {
      set src  "node:[$cfg(g) arc source $arc]"
      set dest "node:$n"
      set newArc [addNewArc $w $src $dest]
      foreach what {rules actions} {
        foreach thing [$cfg(g) arc get $arc $what] {
           $cfg(g) arc lappend $newArc $what $thing
        }
      }     
   }  

   foreach arc [$cfg(g) arcs -out $srcNode] {
      set src  "node:$n"
      set dest "node:[$cfg(g) arc target $arc]"
      set newArc [addNewArc $w $src $dest]
      foreach what {rules actions} {
        foreach thing [$cfg(g) arc get $arc $what] {
           $cfg(g) arc lappend $newArc $what $thing
        }
      }     
   }  
}

proc deleteNode {w tag} {
   upvar #0 $w cfg
   set node [lindex [split $tag :] 1] 
   foreach arcTag [$cfg(g) arcs -adj $node] {
      $cfg(c) delete arc:$arcTag
   }
   $cfg(c) delete $tag
   $cfg(g) node delete $node
   set cfg(selected) {}
   select $w 0 0
}

proc postAddArcMenu {w srcTag destTag x y cx cy} {
   upvar #0 $w cfg
   catch {destroy $cfg(c).addArcMenu}
   menu $cfg(c).addArcMenu -tearoff 0
   set srcName [lindex [split $srcTag :] 1]; set destName [lindex [split $destTag :] 1]
   
   if {![catch {set src [$cfg(g) node get $srcName name]; 
                set dest [$cfg(g) node get $destName name]}]} {
      $cfg(c).addArcMenu add command \
          -label "Add arc from $src to $dest" \
          -command "addNewArc $w $srcTag $destTag"
      tk_popup $cfg(c).addArcMenu $x $y 
   }  
}

proc addNewArc {w srcTag destTag} {
   upvar #0 $w cfg
   set n 0; while {[$cfg(g) arc exists arc$n]} {incr n}
   set arc "arc$n"
   set c1 [$cfg(c) coords $srcTag]
   set x1 [expr ([lindex $c1 0] + [lindex $c1 2])/2]
   set y1 [expr ([lindex $c1 1] + [lindex $c1 3])/2]
   set c2 [$cfg(c) coords $destTag]
   set x2 [expr ([lindex $c2 0] + [lindex $c2 2])/2]
   set y2 [expr ([lindex $c2 1] + [lindex $c2 3])/2]
   set mx [expr ($x1 + $x2) / 2.0]; set my [expr ($y1 + $y2) / 2.0]
   $cfg(c) create line $x1 $y1 $mx $my $x2 $y2 -smooth bezier -tags arc:$arc -arrow last -arrowshape {15 25 10}
   
   $cfg(g) arc insert [lindex [split $srcTag :] 1] [lindex [split $destTag :] 1] $arc
   $cfg(g) arc lappend $arc rules   {}
   $cfg(g) arc lappend $arc actions {}
   $cfg(g) arc set $arc x $mx
   $cfg(g) arc set $arc y $my
   $cfg(c) configure -scrollregion [$cfg(c) bbox all]

   return $arc
}

proc deleteArc {w tag} {
   upvar #0 $w cfg
   $cfg(c) delete $tag
   $cfg(g) arc delete [lindex [split $tag :] 1]
}

proc dupArc {w tag} {
   upvar #0 $w cfg
   set arc [lindex [split $tag :] 1]
   set src  "node:[$cfg(g) arc source $arc]"
   set dest "node:[$cfg(g) arc target $arc]"
   set newArc [addNewArc $w $src $dest]

   foreach rule [$cfg(g) arc get $arc rules] {
     $cfg(g) arc lappend $newArc rules  $rule
   }  
   foreach action [$cfg(g) arc get $arc actions] {
     $cfg(g) arc lappend $newArc actions $action
   }
}

proc postChangeMenu {w tag x y cx cy} {
   upvar #0 $w cfg
   set isNode [string match "node*" $tag]
   catch {destroy $cfg(c).postChangeMenu}
   menu $cfg(c).postChangeMenu -tearoff 0
   if {$isNode} {
      $cfg(c).postChangeMenu add command -label "Delete" -command "deleteNode $w $tag"
      $cfg(c).postChangeMenu add command -label "Duplicate" -command "dupNode $w $tag"
   } else {
      $cfg(c).postChangeMenu add command -label "Delete" -command "deleteArc $w $tag"
      $cfg(c).postChangeMenu add command -label "Duplicate" -command "dupArc $w $tag"
   }
   tk_popup $cfg(c).postChangeMenu  $x $y
}

# Create the UI for the current object and fill it out 
proc showSelection {w type name} {
   upvar #0 $w cfg

   # Save old edits
   if {$cfg(edit) != {}} {
      catch {set_$cfg(editType) $w}
   }
   if {"$cfg(s)" != "$w.p.$type"} {
      catch {$w.p forget $cfg(s)}
      $w.p add $w.p.$type
      set cfg(s) "$w.p.$type"
   }
   show_$type $w 
}

proc setup_help {w junk} {
   upvar #0 $w cfg
   set f [text $w.p.help -width 70 -height 10]
   $f insert end {<left-click>: select a node or arc.
<right-click>:  shows a context-sensitive  menu.

Once a node/arc is selected, it can be dragged to a new position.

Nodes are created by right-clicking on a blank area.

Arcs are created by firstly selecting a source node, then right-
clicking over a target node.
}
   return $f
}
proc show_help {w} {}

# UI for node editing
proc setup_node {w name} {
   upvar #0 $w cfg
   set f [frame $w.p.node]
   label $f.l1 -text "Name" 
   entry $f.e1 -textvariable $w\(name\) -width 12 -validate key -vcmd {string is wordchar %P}
   label $f.l2 -text "Alias" 
   entry $f.e2 -textvariable $w\(alias\) -width 12 -validate key -vcmd {string is wordchar %P}
   label $f.l3 -text "Description" 
   entry $f.e3 -textvariable $w\(desc\) -width 40 
   label $f.l4 -text "Colour" 
   entry $f.e4 -textvariable $w\(fill\) -width 12 

   grid $f.l1 -row 1 -column 1 -sticky w
   grid $f.e1 -row 1 -column 2 -sticky w
   grid $f.l2 -row 2 -column 1 -sticky w
   grid $f.e2 -row 2 -column 2 -sticky w
   grid $f.l3 -row 3 -column 1 -sticky w
   grid $f.e3 -row 3 -column 2 -sticky ew
   grid $f.l4 -row 4 -column 1 -sticky w
   grid $f.e4 -row 4 -column 2 -sticky w
   grid columnconf $f 3 -weight 1
   grid rowconf    $f 6 -weight 1

   return $f
}

proc show_node {w} {
   upvar #0 $w cfg
   set node [lindex [split $cfg(selected) :] 1]
   set cfg(name) [$cfg(g) node get $node name]
   set cfg(alias) [$cfg(g) node get $node alias]
   set cfg(desc) [$cfg(g) node get $node desc]
   set cfg(fill) [$cfg(g) node get $node fill]

   set cfg(message) "State $cfg(name)"
   set cfg(edit) $node; set cfg(editType) "node"
}

proc set_node {w} {
   upvar #0 $w cfg
   set node $cfg(edit)
   if {$node == {}} {return}
   
   # Make the node name unique: append a "_1", "_2" etc.. if needed.
   set candidate $cfg(name); set found 1
   while {$found} {
      set found 0
      foreach otherNode [$cfg(g) nodes] {
         if {"$node" != "$otherNode" && 
             [string equal [$cfg(g) node get $otherNode name] $candidate]} {
            set found 1
         }
      }
      if {$found} {
         set num [lindex [split $candidate "_"] end]
         if {! [string is integer -strict $num]} {
           set candidate "${candidate}_1"
         } else {
           set root [join [lrange [split $candidate "_"] 0 end-1] "_"]
           set candidate "${root}_[expr 1+$num]"
         }
      }
   }

   $cfg(g) node set $node name [set cfg(name) $candidate]
   $cfg(g) node set $node alias $cfg(alias)
   $cfg(g) node set $node desc $cfg(desc)
   $cfg(g) node set $node fill $cfg(fill)
   foreach id [$cfg(c) find withtag node:$node] {
     if {[$cfg(c) type $id] == "text"}  {$cfg(c) itemconf $id -text $cfg(name)}
     if {[$cfg(c) type $id] == "oval"}  {$cfg(c) itemconf $id -fill $cfg(fill)}
   }  
   set cfg(edit) {}
}

proc setup_arc {w name} {
   upvar #0 $w cfg
   set f [frame $w.p.arc]
   set p [panedwindow $f.p -orient vertical]

   frame $p.r
   label $p.r.l1 -text "Rules" 
   set cfg(rulesW) [text $p.r.rules -height 4 -wrap none  -yscrollcommand "$p.r.rsbar set"]
   scrollbar $p.r.rsbar -orient vertical -command "$p.r.rules yview"
   grid $p.r.l1    -row 0 -column 0 -sticky w
   grid $p.r.rules -row 1 -column 0 -sticky nsew
   grid $p.r.rsbar -row 1 -column 1 -sticky w
   grid rowconfig    $p.r 1 -weight 1
   grid columnconfig $p.r 0 -weight 1

   frame $p.a
   label $p.a.l2 -text "Actions" 
   set cfg(actionsW) [text $p.a.actions -height 3 -wrap none  -yscrollcommand "$p.a.asbar set"]
   scrollbar $p.a.asbar -orient vertical -command "$p.a.actions yview"
   grid $p.a.l2      -row 0 -column 0 -sticky w
   grid $p.a.actions -row 1 -column 0 -sticky nsew
   grid $p.a.asbar   -row 1 -column 1 -sticky w 
   grid rowconfig    $p.a 1 -weight 1
   grid columnconfig $p.a 0 -weight 1

   $p add $p.r
   $p paneconfigure $p.r  -sticky nsew
   $p add $p.a 
   $p paneconfigure $p.a  -sticky nsew

   grid $f.p -row 0 -column 0 -sticky nsew
   grid rowconfig $f 0 -weight 1
   grid columnconfig $f 0 -weight 1

   #if {[info exists cfg(rules_height)]} {$p sash place 0 2 $cfg(rules_height)}
   #bind $p <ButtonRelease-1> "+ set $w\(rules_height\) \[lindex \[$p sash coord 0\] 1\]; set $w\(message\) \[lindex \[$p sash coord 0\] 1\]"
   return $f
}
proc show_arc {w} {
   upvar #0 $w cfg
   set arc [lindex [split $cfg(selected) :] 1]

   $cfg(rulesW) delete 0.0 end
   foreach rule [$cfg(g) arc get $arc rules] {
      $cfg(rulesW) insert end "$rule\n"
   }
   $cfg(actionsW) delete 0.0 end
   foreach action [$cfg(g) arc get $arc actions] {
      $cfg(actionsW) insert end "$action\n"
   }
   set src [$cfg(g) node get [$cfg(g) arc source $arc] name]
   set dest [$cfg(g) node get [$cfg(g) arc target $arc] name]
   set cfg(message) "Transition from $src to $dest"

   set cfg(edit) $arc; set cfg(editType) "arc"
}

proc set_arc {w} {
   upvar #0 $w cfg
   set arc $cfg(edit)
   if {$arc != {}} {
      $cfg(g) arc set $arc rules {}
      foreach rule [split [$cfg(rulesW) get 0.0 end] "\n"] {
         if {$rule != {}} {$cfg(g) arc lappend $arc rules $rule}
      }
      $cfg(g) arc set $arc actions {}
      foreach action [split [$cfg(actionsW) get 0.0 end] "\n"] {
         if {$action != {}} {$cfg(g) arc lappend $arc actions $action}
      }
   }
   set cfg(edit) {}
}

# Save the graph to an XML tree. Makes both our node/ arc  objects, and an invisible rule
# for the manager.
proc saveGraph {w} {
   upvar #0 $w cfg

   if {!$cfg(setupOk)} {error "Initialisation failed - Not saving"}

   # Clean up any edits in progress
   if {$cfg(edit) != {}} {
      catch {set_$cfg(editType) $w}
   }

   set x "<tclui name=\"[$cfg(docroot) getAttribute name]\">\n"
   append x "<uiscript><!\[CDATA\[[getValue $cfg(docroot) uiscript]\]\]></uiscript>\n"
   foreach what {canvas_height rules_height} {
      append x "<$what>$cfg($what)</$what>\n"
   }

   set rule "<rule name=\"Init rule\" invisible=\"yes\" condition=\"init\">\n"
   append rule "<!\[CDATA\[package require struct\n"
   append rule "::struct::graph stateGraph\n"
   
   foreach node [$cfg(g) nodes] {
      append x "<node>\n"
      foreach key [$cfg(g) node keys $node] {
         set value [$cfg(g) node get $node $key]
         append x " <$key>$value</$key>\n"
      }
      append x "</node>\n"
      append rule "stateGraph node insert \"[$cfg(g) node get $node name]\"\n"
      append rule "set alias([$cfg(g) node get $node name]) \"[$cfg(g) node get $node alias]\"\n"
      append rule "set colour([$cfg(g) node get $node name]) \"[$cfg(g) node get $node fill]\"\n"
   }
   foreach arc [$cfg(g) arcs] {
      append x "<arc><name>$arc</name>\n"
      set sourceNodeName [$cfg(g) node get [$cfg(g) arc source $arc] name]
      append x " <source>$sourceNodeName</source>\n"
      set targetNodeName [$cfg(g) node get [$cfg(g) arc target $arc] name]
      append x " <target>$targetNodeName</target>\n"
      append rule "stateGraph arc insert \"$sourceNodeName\" \"$targetNodeName\" \"$arc\"\n"

      foreach {key value} [$cfg(g) arc getall $arc] {
         if {$key != "rules" && $key != "actions"} {
            append x " <$key>$value</$key>\n"
         } else {
            foreach thing [$cfg(g) arc get $arc $key] {
               if {$thing != {}} {
                  append rule "stateGraph arc lappend \"$arc\" $key \{$thing\}\n"
                  set thing [string map {\< "&lt;"  \> "&gt;" 
                                         \& "&amp;" \" "&quot;" 
                                         \[ "&#91;" \] "&#93;"} $thing]
                  append x " <$key>$thing</$key>\n"
               }   
            }
         }
      }
      append x " <rules/><actions/>\n"
      append x "</arc>\n"
      append rule "stateGraph arc lappend \"$arc\" actions \{\}\n"
   }

   append x "$rule\]\]></rule>\n"
   append x "</tclui>\n"

   return $x
}

# Read the graph from an XML tree
proc openGraph {w} {
   upvar #0 $w cfg
   catch {$cfg(g) destroy}
   catch {g destroy}
   set cfg(g) {}
   set cfg(s) {}
   set cfg(edit) {}
   set cfg(setupOk) 0

   set errorsFound 0

   set cfg(g) [::struct::graph g]

   global XMLDoc 
   set cfg(doc) [dom parse $XMLDoc]
   set cfg(docroot) [$cfg(doc) documentElement]

   set cfg(canvas_height) [getValue $cfg(docroot) canvas_height]; if {$cfg(canvas_height) == {}} {set cfg(canvas_height) 400}
   set cfg(rules_height) [getValue $cfg(docroot) rules_height]; if {$cfg(rules_height) == {}} {set cfg(rules_height) 40}
   
   foreach node [$cfg(docroot) selectNodes //node] {
      $cfg(g) node insert [set nodeName [getValue $node name]]
      $cfg(g) node set $nodeName name $nodeName
      foreach {what default} {x1 100 x2 200 y1 100 y2 200 fill purple desc "No description" alias zzz} {
         if {[set value [getValue $node $what]] == ""} {
            $cfg(g) node set $nodeName $what $default
         } else {
            $cfg(g) node set $nodeName $what $value
         }
      }
   }

   foreach arc [$cfg(docroot) selectNodes //arc] {
      set arcName [getValue $arc name]
      set sourceNode [getValue $arc source]
      set targetNode [getValue $arc target]
      if {[$cfg(g) node exists $sourceNode] && [$cfg(g) node exists $targetNode]} {
         $cfg(g) arc insert $sourceNode $targetNode $arcName 
         foreach {what default} {x 300 y 300} {
            if {[set value [getValue $arc $what]] == ""} {
                $cfg(g) arc set $arcName $what $default
            } elseif {$what != "name"} {
                $cfg(g) arc set $arcName $what $value
            }
         }
         foreach {what} {rules actions} {
            foreach value [getValues $arc $what] {
               set value [string map {"&lt;"  \<  "&gt;" \> 
                                      "&amp;" \&  "&quot;"  \"
                                      "&#91;" \[  "&#93;" \] } $value]
         
               $cfg(g) arc lappend $arcName $what $value
            }
         }
      } else {
         tk_messageBox -title "Whoops!!" -message "source ($sourceNode) and/or target ($targetNode) is missing??" -type ok
         set errorsFound 1
      }   
   }
   set cfg(setupOk) [expr ! $errorsFound]
}    

# Create a graph on a canvas
proc createGraph {w} {
   upvar #0 $w cfg
   foreach id [$cfg(c) find all] {$cfg(c) delete $id}
   foreach n [$cfg(g) nodes] {
      foreach what {x1 y1 x2 y2 name fill} {set $what [$cfg(g) node get $n $what]}
      $cfg(c) create oval $x1 $y1 $x2 $y2 -fill $fill -tags node:$n
      set mx($n) [expr ($x1 + $x2) / 2.0]; set my($n) [expr ($y1 + $y2) / 2.0]
      $cfg(c) create text $mx($n) $my($n) -text $name -anchor center -tags node:$n
   }

   foreach a [$cfg(g) arcs] {
      set n1 [$cfg(g) arc source $a]
      set n2 [$cfg(g) arc target $a]
      $cfg(c) create line 0 0 0 0 -smooth bezier -tags arc:$a -arrow last -arrowshape {15 25 10}
      fixupArrow $w arc:$a "node:$n1" "node:$n2"
   }
   $cfg(c) configure -scrollregion [$cfg(c) bbox all]

   bind $cfg(c) <Button-1>  "+select $w \[$cfg(c) canvasx %x\] \[$cfg(c) canvasy %y\]"
   bind $cfg(c) <B1-Motion> "+move $w   \[$cfg(c) canvasx %x\] \[$cfg(c) canvasy %y\]"
   bind $cfg(c) <Button-3>  "+action $w %X %Y \[$cfg(c) canvasx %x\] \[$cfg(c) canvasy %y\]"
   set cfg(selected) {}

   setup_node $w dummy
   setup_arc  $w dummy
   setup_help $w dummy
   showSelection $w help dummy
}

proc setupUI {w} {
  panedwindow $w.p -orient vertical
  frame $w.t
  scrollbar $w.t.hs -orient horiz -command "$w.t.c xview"
  scrollbar $w.t.vs -orient vert  -command "$w.t.c yview"
  global $w
  set $w\(c\) [canvas $w.t.c -xscrollcommand "$w.t.hs set" -yscrollcommand "$w.t.vs set"]
  label $w.t.m -width 40 -textvariable $w\(message\) -anchor w
  
  grid $w.t.c  -row 0 -column 0 -sticky nsew
  grid $w.t.hs -row 1 -column 0 -sticky ew
  grid $w.t.vs -row 0 -column 1 -sticky ns
  grid $w.t.m  -row 2 -column 0 -sticky ew
  grid columnconf $w.t 0 -weight 1
  grid rowconf    $w.t 0 -weight 1
  $w.p add $w.t    -sticky nswe

  grid $w.p -in $w -row 0 -column 0 -sticky nsew
  grid columnconf $w 0 -weight 1
  grid rowconf    $w 0 -weight 1
}

##############################
catch {unset .w}
catch {destroy .w}
set w [frame .w]

if {[catch {openGraph $w}]} {tk_messageBox -title "openGraph" -message "$errorInfo" -type ok}
            
setupUI $w

if {[catch {createGraph $w}]} {tk_messageBox -title "createGraph" -message "$errorInfo" -type ok}

grid forget .
grid .w -row 0 -column 0 -sticky nwes
grid columnconf . 0 -weight 1
grid rowconf    . 0 -weight 1
grid columnconf . 1 -weight 0
grid rowconf    . 1 -weight 0
update

if {[info exists .w(canvas_height)]} {.w.p sash place 0 2 [set .w(canvas_height)]}
bind .w.p <Leave> "+ set .w\(canvas_height\) \[lindex \[.w.p sash coord 0\] 1\]"

trace add variable XMLDoc read setXML
proc setXML {name1 name2 op} {
   if {[catch {set newXML [saveGraph .w]}]} {
      global errorInfo
      tk_messageBox -title "Error - not saved" -message $errorInfo -type ok
   }  else {
      global XMLDoc
      set XMLDoc $newXML
   }
}   

bind . <Escape> {+console show}         ;# quick restart
