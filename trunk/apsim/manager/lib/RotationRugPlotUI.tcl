# Displays a rug plot of crop rotations

trace remove variable XMLDoc read setXML
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
   global XMLDoc
   set data(doc) [dom parse $XMLDoc]
   set data(docroot) [$data(doc) documentElement]
   set data(filename) [getValue $data(docroot) filename]
}   

# Read in the logfile
proc readIn {w} {
   upvar #0 $w data
   
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
            set data($paddock,timeline) [list [list $data(earliest) fallow]]
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
      } else {
         append data(text,$date) "$line\n"
      }
   }
   close $fp
   if {![info exists data(earliest)]} {set data(message) "No 'Starting' tag found - is this really a logfile??"; return 0}
   if {$data(finishedOK) == 0} {set data(message) "No 'Finished' tag found - did the run crash??";return 0}

   foreach paddock $data(paddocks) {
      lappend data($paddock,timeline) [list $data(latest) end]
   }

   set message "Finished Reading."; update
   return 1
}

proc setupUI {w} {
   upvar #0 $w data

   panedwindow $w.p -orient h
   
   # Filename selector
   frame $w.f
   label $w.f.l -text "Filename"
   entry $w.f.e -textvariable $w\(filename\)
   button $w.f.b -text Browse -command "browseFilename $w"
   grid $w.f.l  -row 0 -column 0 -sticky w
   grid $w.f.e  -row 0 -column 1 -sticky ew
   grid $w.f.b  -row 0 -column 2 -sticky w
   grid columnconf $w.f 1 -weight 1

   # The rugplot canvas
   set data(canvas) [canvas $w.p.c]

   # Scrollable text window
   frame $w.p.t
   set data(text) [text $w.p.t.t -takefocus 0 -font "normal 14" -yscrollcommand "$w.p.t.sb set"]
   scrollbar $w.p.t.sb -orient v -command "$w.p.t.t yview"
   grid $w.p.t.t  -row 0 -column 0 -sticky nsew
   grid $w.p.t.sb -row 0 -column 1 -sticky nsw

   message $w.m  -textvariable $w\(message\) -aspect 1000 -anchor w

   $w.p add $w.p.c -sticky nsew
   $w.p add $w.p.t -sticky new

   grid $w.f -in $w -row 0 -column 0 -sticky ew
   grid $w.p -in $w -row 1 -column 0 -sticky nsew
   grid $w.m -in $w -row 2 -column 0 -sticky ew

   bind $w.p.c <Motion> "catch \"canvasMotion $w %x %y\""
   bind $w.p.c <Button-1>      "+focus %W; catch \"canvasButton $w %x %y\""
   bind $w.p.c <Key-Up>     "+catch \"canvasUp $w\""
   bind $w.p.c <Key-Down>   "+catch \"canvasDown $w\""
   bind $w.p.c <Key-Left>   "+catch \"canvasLeft $w\""
   bind $w.p.c <Key-Right>  "+catch \"canvasRight $w\""
   bind $w.p.c <Configure>  "+catch \"draw $w\""
   
   focus $w.p.c
   return 1
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

proc min {a b} { return [expr ($a < $b) ? $a : $b] }
proc max {a b} { return [expr ($a > $b) ? $a : $b] }

# Coordinate transforms
proc ctowx {w cx} {
   upvar #0 $w data
   set colwidth [expr ($data(width)-$data(xMargin))/[llength $data(paddocks)]]
   return [max [expr ($cx-$data(xMargin))/$colwidth] 0]
}
proc wtocx {w wx} {
   upvar #0 $w data
   set colwidth [expr ($data(width)-$data(xMargin))/[llength $data(paddocks)]]
   return [expr $data(xMargin) + $wx * $colwidth]
}
proc wtocy {w wy} {
   upvar #0 $w data
   return [expr 20 + ([date2num $wy]+$data(offsetY))*$data(scaleFactY) ]
}
proc ctowy {w cy} {
   upvar #0 $w data
   return [expr (($cy-20)/$data(scaleFactY))-$data(offsetY) ]
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

# Canvas bindings - mouse and keys
proc canvasMotion {w x y} {
   upvar #0 $w data
   set time [split [num2date [ctowy $w $y]] ","]
   set dstr ""
   catch {
     set dstr [clock format [clock scan "1/1/[lindex $time 1] + [expr [lindex $time 0]-1] days"] -format "%d-%b-%Y"]
   }
   set paddockName [lindex $data(paddocks) [ctowx $w $x]]
   set data(message) "$paddockName ; date=$dstr"
   update
}
proc canvasButton {w x y} {
   upvar #0 $w data
   set data(currTime) [num2date [ctowy $w $y]]
   set data(currPaddock) [lindex $data(paddocks) [ctowx $w $x]]
   showText $w
}
proc canvasUp {w} {
   upvar #0 $w data
   foreach {day year} [split $data(currTime) ","] {break}
   set day [expr $day-1]
   if {$day <= 0} {set day 366; set year [expr $year-1]}
   set data(currTime) "$day,$year"
   showText $w
}
proc canvasDown {w} {
   upvar #0 $w data
   foreach {day year} [split $data(currTime) ","] {break}
   set day [expr $day+1]
   if {$day > 366} {set day 1; set year [expr $year+1]}
   set data(currTime) "$day,$year"
   showText $w
}
proc canvasLeft {w} {
   upvar #0 $w data
   set paddock [lsearch $data(paddocks) $data(currPaddock)]
   set paddock [expr $paddock-1]
   if {$paddock < 0} {set paddock [expr [llength $data(paddocks)]-1]}
   set data(currPaddock) [lindex $data(paddocks) $paddock]
   showText $w
}
proc canvasRight {w} {
   upvar #0 $w data
   set paddock [lsearch $data(paddocks) $data(currPaddock)]
   set paddock [expr $paddock+1]
   if {$paddock > [expr [llength $data(paddocks)]-1]} {set paddock 0}
   set data(currPaddock) [lindex $data(paddocks) $paddock]
   showText $w
}

# Display the current ruleset in the text window
proc showText {w} {
   upvar #0 $w data
   set stime [split $data(currTime) ","]
   set year [lindex $stime 1]; set day [max [expr [lindex $stime 0]-1] 0]
  
   set dstr [clock format [clock scan "1/1/$year + $day days"] -format "%d-%b-%Y"]
   $data(text) delete 0.0 end
   $data(text) insert end "$dstr ($data(currTime))\n"
   if {[info exists data(text,$data(currTime))]} {
      $data(text) insert end $data(text,$data(currTime))
   } 
   
   set paddock [lsearch $data(paddocks) $data(currPaddock)]
   $data(canvas) coords currPos [wtocx $w 0] [wtocy $w $data(currTime)] [wtocx $w [llength $data(paddocks)]] [wtocy $w $data(currTime)] 
}

# Draw the paddock history
proc draw {w} {
   upvar #0 $w data

   $data(canvas) delete all
   
   set data(height) [winfo height $data(canvas)]
   set data(width)  [winfo width $data(canvas)]
   set data(xMargin) 60
   
   set t0 [date2num $data(earliest)]
   set t1 [date2num $data(latest)]
   set data(scaleFactY) [expr ($data(height)-20)/($t1 - $t0)]
   set data(offsetY) [expr - $t0]

   # boxes
   set column 0
   foreach paddock $data(paddocks) {
      for {set i 1} {$i < [llength $data($paddock,timeline)]} {incr i} {
         set t0 [lindex [lindex $data($paddock,timeline) [expr $i-1]] 0]
         set t1 [lindex [lindex $data($paddock,timeline)       $i   ] 0]
         set what [lindex [lindex $data($paddock,timeline) [expr $i-1]] 1]
         if {[info exists data(colours,$what)]} {set colour $data(colours,$what)} else {set colour white}
         $data(canvas) create rect [wtocx $w $column] [wtocy $w $t0] [wtocx $w [expr $column+1]] [wtocy $w $t1] -fill $colour -outline {}
      }
      incr column 
   }
   # text
   set column 0
   foreach paddock $data(paddocks) {
      for {set i 1} {$i < [llength $data($paddock,timeline)]} {incr i} {
         set t0 [lindex [lindex $data($paddock,timeline) [expr $i-1]] 0]
         set t1 [lindex [lindex $data($paddock,timeline)       $i   ] 0]
         set what [lindex [lindex $data($paddock,timeline) [expr $i-1]] 1]
         $data(canvas) create text [expr 5 + [wtocx $w $column]] [wtocy $w $t0] -text $what -anchor nw
      }
      incr column 
   }
   # column boundaries
   for {set col 0} {$col < [llength $data(paddocks)]} {incr col} {
      $data(canvas) create line [wtocx $w $col] [wtocy $w $data(earliest)] [wtocx $w $col] [wtocy $w $data(latest)] -fill lightgray
   }
   # year boundaries
   for {set y [lindex [split $data(earliest) ","] 1]} {$y <= [lindex [split $data(latest) ","] 1]} {incr y} {
      $data(canvas) create line [wtocx $w 0] [wtocy $w "1,$y"] [wtocx $w [llength $data(paddocks)]] [wtocy $w "1,$y"] -fill lightgray
      $data(canvas) create text 5 [wtocy $w "1,$y"] -text "1/1/$y" -anchor w
   }
   # top margin text
   set y [wtocy $w $data(earliest)]
   for {set x 0} {$x < [llength $data(paddocks)]} {incr x} {
      $data(canvas) create text [expr 20 + [wtocx $w $x]] $y -text "[lindex $data(paddocks) $x]" -anchor sw
   }
   #Current position
   $data(canvas) create line [wtocx $w 0] [wtocy $w $data(earliest)] [wtocx $w 1] [wtocy $w $data(earliest)] -fill white -tag currPos
   set data(message) "Finished Drawing. ($data(width)x$data(height))"
}

##############################
########Main program starts here
##############################
catch {unset .w}
catch {destroy .w}

frame      .w
appletInit .w
setupUI    .w
if {[readIn .w]} {draw .w}

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
