# Displays a rug plot of crop rotations

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
   global XMLDoc
   set data(doc) [dom parse $XMLDoc]
   set data(docroot) [$data(doc) documentElement]
   set data(filename) [getValue $data(docroot) filename]
}   

# Read in the logfile
proc readIn {w} {
   upvar #0 $w data
   set data(finishedOK) 0
   
   if {![file exists $data(filename)]} { set data(message) "$data(filename) does not exist."; return }

   set data(message) "Reading."; update

   set fp [open $data(filename) r]
   set data(crops) {fallow}; set data(paddocks) {}
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
         append data(text,$date) "$line\n"
      } else {
         append data(text,$date) "$line\n"
      }
   }
   close $fp
   if {![info exists data(earliest)]} {set data(message) "No 'Starting' tag found - is this really a logfile??"; return }
   if {$data(finishedOK) == 0} {set data(message) "No 'Finished' tag found - did the run crash??";return }
   
   foreach paddock $data(paddocks) {
      lappend data($paddock,timeline) [list $data(latest) end]
      $data(tree) insert end root $paddock -text "$paddock"
   }
   foreach {idx colour} [array get data colours,*] {
      set state [lindex [split $idx ","] 1]
      image create photo img.$state -width 15 -height 15 
      img.$state put $colour -to 0 0 15 15
   }
   image create photo img.zero -width 15 -height 15 
   img.zero put red -to 0 0 15 15
   image create photo img.nonzero -width 15 -height 15 
   img.nonzero put lightgreen -to 0 0 15 15

   set data(currTime) $data(earliest)
   set data(currPaddock) [lindex $data(paddocks) 0]
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
   frame $w.p.c
   set data(canvas) [canvas $w.p.c.c]
   set data(canvasScrollbar) [scrollbar $w.p.c.sb -orient v -command "scrollRug $w"]
   grid $w.p.c.c  -row 0 -column 0 -sticky nsew
   grid $w.p.c.sb -row 0 -column 1 -sticky nsw
   grid rowconf $w.p.c 0 -weight 1
   grid columnconf $w.p.c 0 -weight 1
   grid $w.p.c  -row 0 -column 0 -sticky nsew

   # Scrollable text window
   frame $w.p.t
   label $w.p.t.m  -textvariable $w\(message\) -anchor w
   set data(tree) [Tree $w.p.t.t -relief flat -borderwidth 0 \
                     -width 35 -height 15 -highlightthickness 0 \
                     -yscrollcommand "$w.p.t.sb set"]
   scrollbar $w.p.t.sb -orient v -command "$w.p.t.t yview"
   grid $w.p.t.m  -row 0 -column 0 -sticky ew
   grid $w.p.t.t  -row 1 -column 0 -sticky nsew
   grid $w.p.t.sb -row 1 -column 1 -sticky nsw
   grid rowconf $w.p.t 1 -weight 1
   grid columnconf $w.p.t 0 -weight 1
   grid $w.p.t  -row 0 -column 0 -sticky nsew

   $w.p add $w.p.c -sticky nsew
   $w.p add $w.p.t -sticky nsew

   grid $w.f -in $w -row 0 -column 0 -sticky ew
   grid $w.p -in $w -row 1 -column 0 -sticky nsew

   grid rowconf $w 1 -weight 1
   grid columnconf $w 0 -weight 1

#   bind $w.p.c.c <Motion>     "canvasMotion $w %x %y"
   bind $w.p.c.c <Button-1>   "+focus %W; canvasButton $w %x %y"
   bind $w.p.c.c <Key-Up>     "canvasUp $w"
   bind $w.p.c.c <Key-Down>   "canvasDown $w"
   bind $w.p.c.c <Key-Left>   "canvasLeft $w"
   bind $w.p.c.c <Key-Right>  "canvasRight $w"
   bind $w.p.c.c <Configure>  "draw $w"

   bind $w.p.t.t <<TreeSelect>> "treeSelect $w $w.p.t.t"
   
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

   if {!$data(finishedOK)} {return}

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

   if {!$data(finishedOK)} {return}

   set data(currTime) [num2date [ctowy $w $y]]
   set data(currPaddock) [lindex $data(paddocks) [ctowx $w $x]]
   showText $w  1
}
proc canvasUp {w} {
   upvar #0 $w data

   if {!$data(finishedOK)} {return}

   foreach {day year} [split $data(currTime) ","] {break}
   set day [expr $day-1]
   if {$day <= 0} {set day 366; set year [expr $year-1]}
   set data(currTime) "$day,$year"
   showText $w  1
}
proc canvasDown {w} {
   upvar #0 $w data

   if {!$data(finishedOK)} {return}

   foreach {day year} [split $data(currTime) ","] {break}
   set day [expr $day+1]
   if {$day > 366} {set day 1; set year [expr $year+1]}
   set data(currTime) "$day,$year"
   showText $w   1
}
proc canvasLeft {w} {
   upvar #0 $w data

   if {!$data(finishedOK)} {return}

   set paddock [lsearch $data(paddocks) $data(currPaddock)]
   set paddock [expr $paddock-1]
   if {$paddock < 0} {set paddock [expr [llength $data(paddocks)]-1]}
   set data(currPaddock) [lindex $data(paddocks) $paddock]
   showText $w  1
}
proc canvasRight {w} {
   upvar #0 $w data

   if {!$data(finishedOK)} {return}

   set paddock [lsearch $data(paddocks) $data(currPaddock)]
   set paddock [expr $paddock+1]
   if {$paddock > [expr [llength $data(paddocks)]-1]} {set paddock 0}
   set data(currPaddock) [lindex $data(paddocks) $paddock]
   showText $w  1
}

proc scrollRug {w args} {
   upvar #0 $w data

   if {!$data(finishedOK)} {return}

   if {[lindex $args 0] == "moveto"} {
     set fract [lindex $args 1]
     set t0 [date2num $data(earliest)]
     set t1 [date2num $data(latest)]
     set t [expr $t0 + $fract * ($t1-$t0)]
     set data(currTime) [num2date $t]     
   } elseif {[lindex $args 0] == "scroll"} {
     set num [lindex $args 1]
     if {[lindex $args 2] == "pages"} {
        set num [expr $num * 30]
     }
     foreach {day year} [split $data(currTime) ","] {break}
     set day [expr $day + $num]
     if {$day <= 0} {set day 366; set year [expr $year-1]}
     if {$day > 366} {set day 1; set year [expr $year+1]}
     set data(currTime) "$day,$year"
   }
   showText $w 0
}

proc treeSelect {w t} {
   upvar #0 $w data

   if {!$data(finishedOK)} {return}

   set selected [lindex [$t selection get] 0]
   if {$selected != ""} {
      while {$selected != "" && [lsearch $data(paddocks) $selected] < 0} {
         set selected [$data(tree) parent $selected]
      }
      if {$selected != ""} {
         set data(currPaddock) $selected
      }
   }  
}
# Display the current ruleset in the text window
proc showText {w doScroll} {
   upvar #0 $w data

   if {!$data(finishedOK)} {return}

   set stime [split $data(currTime) ","]
   set year [lindex $stime 1]; set day [max [expr [lindex $stime 0]-1] 0]
  
   set dstr [clock format [clock scan "1/1/$year + $day days"] -format "%d-%b-%Y"]
   set data(message) "$dstr ($data(currTime))"
   
   foreach paddock $data(paddocks) {
      $data(tree) delete [$data(tree) nodes $paddock]
   }

   if {[info exists data(text,$data(currTime))]} {
      set numEvals 1
      foreach line [split $data(text,$data(currTime)) "\n"] {
         set list [split $line ","]
         if {[llength $list] == 2} { 
           #### "p1,State is Fallow1. (esw=120)"  ###
           set paddock [string trim [lindex $list 0]]
           set msg [string trim [lindex $list 1]]
           set state [string trimright [lindex $msg 2] "."]
           $data(tree) insert [expr $numEvals-1] $paddock #auto -text "<$numEvals>$msg" -image img.$state
         } elseif {[llength $list] == 4} {
           ## "Paddock1,target=Chickpea,rule=[canPlant_chickpea], value=0"
           set paddock [string trim [lindex $list 0]]
           set target [lindex [split [lindex $list 1] "="] 1]
           if {![$data(tree) exists $paddock.$target]} {
              $data(tree) insert end $paddock $paddock.$target -text "$target" -image img.$target
           }   
           set rule   [join [lrange [split [lindex $list 2] "="] 1 end]]
           set value  [join [lrange [split [lindex $list 3] "="] 1 end]]
           if {$value != 0} {
              $data(tree) insert end $paddock.$target #auto -text "<$numEvals>$rule = $value" -image img.nonzero
           } else {
              $data(tree) insert end $paddock.$target #auto -text "<$numEvals>$rule = $value" -image img.zero
           }
         } elseif {[string match changeState* $line]} {
           incr numEvals
         }
      }
   } 
   $data(tree) opentree $data(currPaddock)
   
   set paddock [lsearch $data(paddocks) $data(currPaddock)]
   $data(canvas) coords currPos [wtocx $w 0] [wtocy $w $data(currTime)] [wtocx $w [llength $data(paddocks)]] [wtocy $w $data(currTime)] 

   if {$doScroll} {
     set t [date2num $data(currTime)]
     set t0 [date2num $data(earliest)]
     set t1 [date2num $data(latest)]
     set fract [expr ($t-$t0)/($t1-$t0)]
     $data(canvasScrollbar) set $fract $fract
   }  
}

# Draw the paddock history
proc draw {w} {
   upvar #0 $w data

   if {!$data(finishedOK)} {return}

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
   $data(canvas) create line [wtocx $w 0] [wtocy $w $data(earliest)] [wtocx $w 1] [wtocy $w $data(earliest)] -fill blue -width 3 -tag currPos
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
readIn .w
draw .w

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
