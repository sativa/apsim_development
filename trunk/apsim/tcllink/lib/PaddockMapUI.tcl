# Displays a map of paddocks

trace remove variable XMLDoc read setXML
package req Tk
package req BWidget
package req tdom

# Get the value of one of our parameters
proc getValue {id thing} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         return [$node text]
      }
   }
   return ""
}

# Set the value of our 'thing'
proc setValue {w thing what} {
   upvar #0 $w data
   foreach node [$data(docroot) childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         $data(docroot) removeChild $node
      }
   }
   set node [$data(doc) createElement $thing]
   $node appendChild [$data(doc) createTextNode $what]
   $data(docroot) appendChild $node
}

proc appletInit {w} {
   upvar #0 $w data
   global XMLDoc  apsuite
   set data(doc) [dom parse $XMLDoc]
   set data(docroot) [$data(doc) documentElement]
   set data(filename) [getValue $data(docroot) filename]           ;# logfile
   regsub -all "%apsuite" $data(filename) $apsuite data(filename)
   set data(bgFilename) [getValue $data(docroot) bgFilename]       ;# georeferenced bitmap (unused)
   set data(kmlFilename) [getValue $data(docroot) kmlFilename]     ;# paddock layout
   regsub -all "%apsuite" $data(kmlFilename) $apsuite data(kmlFilename)
}   

# Read in the logfile
proc readIn {w} {
   upvar #0 $w data
   set data(readOK) 0
   
   if {![file exists $data(filename)]} { set data(message) "$data(filename) does not exist."; return 0}

   set data(message) "Reading."; update

   set fp [open $data(filename) r]
   set data(finishedOK) 0
   set data(crops) {fallow}
   set date "X"
   while {[gets $fp line] > 0} {
      if {[scan $line "%d,%d" day year]} {
         set date $line
         set data(latest) $date
      } elseif {[string match "Starting*" $line]} {
         set date [lindex $line 1]
         set data(earliest) $date; set data(latest) $date
      } elseif {[string match "paddocks*" $line]} {
         set data(paddocks) [join [lindex [split $line =] 1]]
         foreach paddock $data(paddocks) {
            set data($paddock,timeline) {}
         }
      } elseif {[string match "Finished*" $line]} {
         set data(finishedOK) 1
      } elseif {[string match colour* $line]} {
         set state [lindex $line 1]
         set data(colours,$state) [lindex $line 2]
      } elseif {[string match changeState* $line]} {
         set paddock [lindex $line 1]
         set state [lindex $line 2]
         lappend data($paddock,timeline) [list $date $state]
         if {[lsearch $data(crops) $state] < 0} {lappend data(crops) $state}
         append data(text,$date) "$line\n"
      } elseif {[string match "*State\ is*" $line]} {
         set paddock [string trim [lindex [split $line ","] 0]]
         if {$data($paddock,timeline) == {}} {
            set state [string trimright [lindex [lindex [split $line ","] 1] 2] "."]
            set data($paddock,timeline) [list [list $date $state]]
         }
      } else {
      }
   }
   close $fp
   if {![info exists data(earliest)]} {set data(message) "No 'Starting' tag found - is this really a logfile??"; return 0}
   if {$data(finishedOK) == 0} {set data(message) "No 'Finished' tag found - did the run crash??";return 0}

   foreach paddock $data(paddocks) {
      lappend data($paddock,timeline) [list $data(latest) end]
   }

   set data(currTime) $data(earliest)
   if {![file exists $data(kmlFilename)]} { set data(message) "$data(kmlFilename) does not exist."; return 0}
   set fp [open $data(kmlFilename) r]
   set doc [dom parse [read $fp]]
   set docroot [$doc documentElement]

 
   set kns {kmlns http://earth.google.com/kml/2.1}
   foreach markerNode [[$doc documentElement] selectNodes -namespaces $kns //kmlns:Placemark] {
      set name [[$markerNode selectNodes -namespaces $kns descendant::kmlns:name] text]
      set i 0
      foreach coordNode [$markerNode selectNodes -namespaces $kns descendant::kmlns:coordinates] {
         set coords {}
         foreach coord [$coordNode text] {
            foreach {x y z} [split $coord ","] {break}
            lappend coords $x $y
         }
         set data(coords,$name,$i) $coords
         incr i
      }
   }
   $doc delete

   foreach paddock $data(paddocks) {
      if {![info exists data(coords,$paddock,0)]} {
         tk_messageBox -type ok -message "paddock \"$paddock\" isn't described in the KML file."
      }
   }

   set data(minX) 1000; set data(maxX) -1000
   set data(minY) 1000; set data(maxY) -1000
   foreach {index coordlist} [array get data coords,*] {
      foreach {x y} $coordlist {
         if {$x < $data(minX)} {set data(minX) $x}
         if {$x > $data(maxX)} {set data(maxX) $x}
         if {$y < $data(minY)} {set data(minY) $y}
         if {$y > $data(maxY)} {set data(maxY) $y}
      }
   }

   set message "Finished Reading."; update
   set data(readOK) 1
}

proc setupUI {w} {
   upvar #0 $w data
   
   # Filename selector
   frame $w.f
   label $w.f.l -text "Log filename"
   entry $w.f.e -textvariable $w\(filename\)
   button $w.f.b -text Browse -command "browseFilename $w"
   label $w.f.kl -text "KML filename"
   entry $w.f.ke -textvariable $w\(kmlFilename\)
   button $w.f.kb -text Browse -command "browseKmlFilename $w"
   grid $w.f.l  -row 0 -column 0 -sticky nw
   grid $w.f.e  -row 0 -column 1 -sticky new
   grid $w.f.b  -row 0 -column 2 -sticky nw
   grid $w.f.kl  -row 1 -column 0 -sticky nw
   grid $w.f.ke  -row 1 -column 1 -sticky new
   grid $w.f.kb  -row 1 -column 2 -sticky nw
   grid columnconf $w.f 1 -weight 1

   # The rugplot canvas
   frame $w.c
   set data(canvas) [canvas $w.c.c]
   set data(canvasScrollbar) [scrollbar $w.c.sb -orient h -command "scrollTime $w"]
   grid $w.c.c  -row 0 -column 0 -sticky nsew
   grid $w.c.sb -row 1 -column 0 -sticky ew

   label $w.m  -textvariable $w\(message\) -anchor w

   grid $w.f  -row 0 -column 0 -sticky ew
   grid $w.c  -row 1 -column 0 -sticky nsew
   grid $w.m  -row 2 -column 0 -sticky ew
   grid rowconf $w.c 0 -weight 1
   grid columnconf $w.c 0 -weight 1

   grid rowconf $w 1 -weight 1
   grid columnconf $w 0 -weight 1

   bind $w.c <Configure>  "draw $w"
}

proc browseFilename {w} {
   set types {
       {{Log Files}  .log }
       {{All Files} * }
   }
   set newFile [tk_getOpenFile -filetypes $types -multiple 0 -title "Choose log file"]
   if {$newFile != ""} {
      upvar #0 $w data
      set data(filename) $newFile
      setValue $w filename $newFile
      if {[readIn $w]} {draw $w}
   }
}

proc browseKmlFilename {w} {
   set types {
       {{KML Files}  .kml }
       {{All Files} * }
   }
   set newFile [tk_getOpenFile -filetypes $types -multiple 0 -title "Choose log file"]
   if {$newFile != ""} {
      upvar #0 $w data
      set data(kmlFilename) $newFile
      setValue $w kmlfilename $newFile
      if {[readIn $w]} {draw $w}
   }
}

proc min {a b} { return [expr ($a < $b) ? $a : $b] }
proc max {a b} { return [expr ($a > $b) ? $a : $b] }

# Set up the coordinate transformations from real world coords to canvas coords.
# Aspect ratio needs to be preserved
proc setXForms {w} {
   upvar #0 $w data

   if {!$data(readOK)} {return}

   set data(width)  [winfo width $w].0
   set data(height) [winfo height $w].0

   set xRange [expr $data(maxX) - $data(minX)]
   set yRange [expr $data(maxY) - $data(minY)]
   set xCellSize [expr $xRange / $data(width)]
   set yCellSize [expr $yRange / $data(height)]
   set cellSize [max $xCellSize $yCellSize]
#   set data(scaleFactX) [expr 1.0/$cellSize]
#   set data(xOffset) [expr -1.0 * $data(minX) * $data(scaleFactX)]
#   set data(scaleFactY) [expr 1.0/$cellSize]
#   set data(yOffset) [expr -1.0 * $data(minY) * $data(scaleFactY)]
   set data(scaleFactX) [expr $data(width)/$xRange]
   set data(xOffset) [expr -1.0 * $data(minX) * $data(scaleFactX)]
   set data(scaleFactY) [expr $data(height)/$yRange]
   set data(yOffset) [expr -1.0 * $data(minY) * $data(scaleFactY)]
}

proc wtocx {w wx} {
   upvar #0 $w data
   return [expr $data(xOffset) + $wx * $data(scaleFactX)]
}
proc ctowx {w cx} {
   upvar #0 $w data
   return [expr ($cx - $data(xOffset)) / $data(scaleFactX)]
}
proc wtocy {w wy} {
   upvar #0 $w data
   return [expr $data(height) - ($data(yOffset) + $wy * $data(scaleFactY))]
}
proc ctowy {w cy} {
   upvar #0 $w data
   return [expr (($cy - $data(height)) + $data(yOffset)) / (-1.0 * $data(scaleFactY))]
}

proc date2num {d} {
   set z [split $d ","]
   return [expr [lindex $z 1]+ [lindex $z 0]/365.0]
}

proc num2date {n} {
   set y [expr int($n)]
   set d [min 366 [max [expr int(($n-$y)*365.0)] 1]]
   return "$d,$y"
}

proc scrollTime {w args} {
   upvar #0 $w data

   if {!$data(readOK)} {return}

   set t0 [date2num $data(earliest)]
   set t1 [date2num $data(latest)]
   if {[lindex $args 0] == "moveto"} {
     set fract [lindex $args 1]
     set t [expr $t0 + $fract * ($t1-$t0)]
     set data(currTime) [num2date $t]     
     $data(canvasScrollbar) set $fract $fract
   } elseif {[lindex $args 0] == "scroll"} {
     set num [lindex $args 1]
     if {[lindex $args 2] == "pages"} {
        set num [expr ($num * 30) / 366.0]
     }
     set t [expr $num + [date2num $data(currTime)]]
     set data(currTime) [num2date $t]
     set fract [expr ($t-$t0)/($t1-$t0)]

     $data(canvasScrollbar) set $fract $fract
   }
   showPaddocks $w
}

# Show the current state of paddocks
proc showPaddocks {w} {
   upvar #0 $w data

   if {!$data(readOK)} {return}

   foreach {day year} [split $data(currTime) ","] {break}
  
   set dstr [clock format [clock scan "1/1/$year + [expr $day-1] days"] -format "%d-%b-%Y"]
   set data(message) "$dstr"
   
   set now [date2num $data(currTime)]
   foreach paddock $data(paddocks) {
      set state ""
      foreach thing $data($paddock,timeline) { 
        foreach {date nextstate} $thing { break }
        set date [date2num $date]
        if {$date > $now} break
        set state $nextstate
      }
      if {[info exists data(colours,$state)]} {
         $data(canvas) itemconf $paddock -fill $data(colours,$state)
      } else {
         $data(canvas) itemconf $paddock -fill {}
      }
   }
}

# Draw the paddocks: Create canvas items.
proc draw {w} {
   upvar #0 $w data

   if {!$data(readOK)} return

   $data(canvas) delete all
   setXForms $w

   foreach paddock $data(paddocks) {
      foreach {index coordList} [array get data coords,$paddock,*] {
         set coords {}
         foreach {x y} $coordList {
            lappend coords [wtocx $w $x] [wtocy $w $y]
         }
         $data(canvas) create polygon $coords -outline black -fill {} -tags $paddock
      }
   }
   showPaddocks $w
   set data(message) "Finished Drawing."
}

##############################
########Main program starts here
##############################
catch {unset .w}
catch {destroy .w}

frame      .w
appletInit .w
setupUI    .w
readIn     .w

grid forget .
grid .w -row 0 -column 0 -sticky nsew
grid columnconf . 0 -weight 1
grid rowconf    . 0 -weight 1
grid columnconf . 1 -weight 0
grid rowconf    . 1 -weight 0

proc setXML {name1 name2 op} {
   upvar #0 .w data
   if {[catch {set newXML [$data(doc) asXML]}]} {
      global errorInfo; tk_messageBox -title "Error - not updating" -message "$errorInfo" -type ok
   } else {
      global XMLDoc
      set XMLDoc $newXML
   }
}
trace add variable XMLDoc read setXML
