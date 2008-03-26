# added extra radiation checks 1/8/2001 PdeV
# freewrap 5.0 & p51s          1/8/2003 PdeV

set TAMETDIR "/development/tools/tamet"

proc freewrap_load {libfile args} {
     # This procedure should be used instead of the normal LOAD command when
     # using binary extensions with freeWrap.
     #
     # Returns: On success, the full file path to the shared library on the local file system.
     #          On failure, an error message starting with the text "Load Error: "
     #
     global env

     set rtnval {}
     set fpath [::freewrap::unpack $libfile]
     if {[string length $fpath] == 0} {
         set rtnval "Load Error: Copying of shared library $libfile failed."
        } {
           if {[file mtime $libfile] > [file mtime $fpath]} {
               # The wrapped library file is newer than the existing one on disk.
               # Let's delete the existing one first, then copy the newer file.
               if {[catch {file delete -force $fpath}]} {
                   set fpath {}
                   set rtnval {Load Error: Unable to delete older copy of shared library.}
                  } {
                     set fpath [::freewrap::unpack $filetemp]
                     if {[string length $fpath] == 0} {
                         set rtnval {Load Error: Unable to replace older copy of shared library.}
                        }
                    }
              }
          }
     if {[string length $rtnval] == 0} {
         # No errors, so far. Let's load the shared library.
         if {[catch "load \{$fpath\} $args" result]} {
             set rtnval "Load Error: $result"
            } { set rtnval $fpath }
        }
     return $rtnval
}

if {0} {
    package require BWidget
    load ./tamet[info sharedlibext]
} else {
    # freewrapped code
    lappend auto_path $TAMETDIR/bwidget1.3.0
    package require BWidget
	 freewrap_load $TAMETDIR/tamet.dll
}

# default parameter values. These should be saved between sessions
set dis_evap   14.0 ;# (mm)
set dish_evap  -8.5 ;# (mm)   ! evap is high with rain
set disvh_evap -12.0 ;# (mm) ! evap is v high with rain
set dis_maxt_dry_season  10.0 ;# (oC) ! maxt   lat -18 to -11, jun to sept
set dis_maxt_other  20.0 ;# (oC) ! maxt
set dis_mint  23.0     ;# (oC)   ! mint
set maxt_to_mint  2.0  ;# (oC)
set rain_lb  0.0    ;# (mm)
set rain_ub  999.9  ;# (mm)
set gauge_capacity  203.2 ;# (mm)
set maxt_lb  4.0    ;# (oC)
set maxt_ub  50.0   ;# (oC)
set mint_lb  -12.5  ;# (oC)
set mint_ub  32.0   ;# (oC)
set evap_lb  0.0    ;# (mm)
set evap_ub  20.0   ;# (mm)
set wind_lb  2.0    ;# (km)
set wind_ub  800.0  ;# (km)
set radn_lb  0.10   ;# ()
set radn_ub  1.12   ;# ()
set radn_low  0.65  ;# ()
set radn_vlow  0.02 ;# ()

set radn_do_extra     0    ;# do extra tests
set radn_cloud_low    0.25
set radn_cloud_high   0.75


proc p_ui {root} {
    if {$root == "."} {
	set base ""
    } else {
	set base $root
    }
    foreach i [winfo chi $root] {destroy $i}

    set tfr [TitleFrame $base.tfr -text "Rain"]
    set f [$tfr getframe]
    label $f.lbl -text "Lower"
    entry $f.lb -width 5 -textvariable rain_lb
    label $f.lbu -text "(mm)"
    label $f.ubl -text "Upper"
    entry $f.ub -width 5 -textvariable rain_ub
    label $f.ubu -text "(mm)"
    label $f.gcl -text "Gauge Capacity"
    entry $f.gc -width 5 -textvariable gauge_capacity
    label $f.gcu -text "(mm)"

    grid $f.lbl -row 0 -column 0 -sticky w
    grid $f.lb  -row 0 -column 1 -sticky w
    grid $f.lbu -row 0 -column 2 -sticky w
    grid $f.ubl -row 1 -column 0 -sticky w
    grid $f.ub  -row 1 -column 1 -sticky w
    grid $f.ubu  -row 1 -column 2 -sticky w
    grid $f.gcl -row 2 -column 0 -sticky w
    grid $f.gc  -row 2 -column 1 -sticky w
    grid $f.gcu  -row 2 -column 2 -sticky w
    grid $tfr -in $root -row 0 -column 0 -padx 2 -pady 2 -sticky new

    set tft [TitleFrame $base.tft -text "Radiation"]
    set f [$tft getframe]
    label $f.lbl -text "Lower"
    entry $f.lb -width 5 -textvariable radn_lb
    label $f.lbu -text "(% clr sky)"
    label $f.ubl -text "Upper"
    entry $f.ub -width 5 -textvariable radn_ub
    label $f.ubu -text "(% clr sky)"
    label $f.ll -text "Low"
    entry $f.l -width 5 -textvariable radn_low
    label $f.lu -text "(% clr sky)"
    label $f.vll -text "Very Low"
    entry $f.vl -width 5 -textvariable radn_vlow
    label $f.vlu -text "(% clr sky)"

    label $f.exl -text "Do extra tests"
    checkbutton $f.exdo -variable radn_do_extra
    label $f.rcll_l -text "Low fraction"
    entry $f.rcll_e -width 5 -textvariable radn_cloud_low
    label $f.rclh_l -text "High fraction"
    entry $f.rclh_e -width 5 -textvariable radn_cloud_high

    grid $f.lbl -row 0 -column 0 -sticky w
    grid $f.lb  -row 0 -column 1 -sticky w
    grid $f.lbu -row 0 -column 2 -sticky w
    grid $f.ubl -row 1 -column 0 -sticky w
    grid $f.ub  -row 1 -column 1 -sticky w
    grid $f.ubu  -row 1 -column 2 -sticky w
    grid $f.ll -row 2 -column 0 -sticky w
    grid $f.l  -row 2 -column 1 -sticky w
    grid $f.lu  -row 2 -column 2 -sticky w
    grid $f.vll -row 3 -column 0 -sticky w
    grid $f.vl  -row 3 -column 1 -sticky w
    grid $f.vlu  -row 3 -column 2 -sticky w
    grid $f.exl -row 4 -column 0 -sticky w
    grid $f.exdo -row 4 -column 1 -sticky w
    grid $f.rcll_l -row 5 -column 0 -sticky w
    grid $f.rcll_e -row 5 -column 1 -sticky w
    grid $f.rclh_l -row 6 -column 0 -sticky w
    grid $f.rclh_e -row 6 -column 1 -sticky w

    grid $tft -in $root -row 0 -column 1 -padx 2 -pady 2 -sticky new

    set tfx [TitleFrame $base.tfx -text "Max. T"]
    set f [$tfx getframe]
    label $f.lbl -text "Lower"
    entry $f.lb -width 5 -textvariable maxt_lb
    label $f.lbu -text "(oC)"
    label $f.ubl -text "Upper"
    entry $f.ub -width 5 -textvariable maxt_ub
    label $f.ubu -text "(oC)"
    label $f.dl -text "Dry season Disc."
    entry $f.d -width 5 -textvariable dis_maxt_dry_season
    label $f.du -text "(oC)"
    label $f.dol -text "Other season Disc."
    entry $f.do -width 5 -textvariable dis_maxt_other
    label $f.dou -text "(oC)"
    grid $f.lbl -row 0 -column 0 -sticky w
    grid $f.lb  -row 0 -column 1 -sticky w
    grid $f.lbu -row 0 -column 2 -sticky w
    grid $f.ubl -row 1 -column 0 -sticky w
    grid $f.ub  -row 1 -column 1 -sticky w
    grid $f.ubu  -row 1 -column 2 -sticky w
    grid $f.dl -row 2 -column 0 -sticky w
    grid $f.d  -row 2 -column 1 -sticky w
    grid $f.du  -row 2 -column 2 -sticky w
    grid $f.dol -row 3 -column 0 -sticky w
    grid $f.do  -row 3 -column 1 -sticky w
    grid $f.dou  -row 3 -column 2 -sticky w
    grid $tfx -in $root -row 1 -column 0 -padx 2 -pady 2 -sticky new

    set tfn [TitleFrame $base.tfn -text "Min. T"]
    set f [$tfn getframe]
    label $f.lbl -text "Lower"
    entry $f.lb -width 5 -textvariable mint_lb
    label $f.lbu -text "(oC)"
    label $f.ubl -text "Upper"
    entry $f.ub -width 5 -textvariable mint_ub
    label $f.ubu -text "(oC)"
    label $f.dl -text "Discontinuity"
    entry $f.d -width 5 -textvariable dis_mint
    label $f.du -text "(oC)"

    grid $f.lbl -row 0 -column 0 -sticky w
    grid $f.lb  -row 0 -column 1 -sticky w
    grid $f.lbu -row 0 -column 2 -sticky w
    grid $f.ubl -row 1 -column 0 -sticky w
    grid $f.ub  -row 1 -column 1 -sticky w
    grid $f.ubu  -row 1 -column 2 -sticky w
    grid $f.dl -row 2 -column 0 -sticky w
    grid $f.d  -row 2 -column 1 -sticky w
    grid $f.du  -row 2 -column 2 -sticky w
    grid $tfn -in $root -row 1 -column 1 -padx 2 -pady 2 -sticky new

    set tfrnge [TitleFrame $base.tfrnge -text "Temp Range"]
    set f [$tfrnge getframe]
    label $f.lbl -text "Maxt to Mint"
    entry $f.lb -width 5 -textvariable maxt_to_mint
    label $f.lbu -text "(oC)"
    grid $f.lbl -row 0 -column 0 -sticky w
    grid $f.lb  -row 0 -column 1 -sticky w
    grid $f.lbu -row 0 -column 2 -sticky w
    grid $tfrnge -in $root -row 2 -column 0 -columnspan 2 \
	-padx 2 -pady 2 -sticky new

    set tfw [TitleFrame $base.tfw -text "Wind"]
    set f [$tfw getframe]
    label $f.lbl -text "Lower"
    entry $f.lb -width 5 -textvariable wind_lb
    label $f.lbu -text "(Km)"
    label $f.ubl -text "Upper"
    entry $f.ub -width 5 -textvariable wind_ub
    label $f.ubu -text "(Km)"
    grid $f.lbl -row 0 -column 0 -sticky w
    grid $f.lb  -row 0 -column 1 -sticky w
    grid $f.lbu -row 0 -column 2 -sticky w
    grid $f.ubl -row 1 -column 0 -sticky w
    grid $f.ub  -row 1 -column 1 -sticky w
    grid $f.ubu  -row 1 -column 2 -sticky w
    grid $tfw -in $root -row 3 -column 1 -padx 2 -pady 2 -sticky new

    set tfe [TitleFrame $base.tfe -text "Evaporation"]
    set f [$tfe getframe]
    label $f.lbl -text "Lower"
    entry $f.lb -width 5 -textvariable evap_lb
    label $f.lbu -text "(mm)"
    label $f.ubl -text "Upper"
    entry $f.ub -width 5 -textvariable evap_ub
    label $f.ubu -text "(mm)"
    label $f.dl -text "Discontinuity"
    entry $f.d -width 5 -textvariable dis_evap
    label $f.du -text "(mm)"
    label $f.dhl -text "Disc. with rain"
    entry $f.dh -width 5 -textvariable dish_evap
    label $f.dhu -text "(mm)"
    label $f.dvhl -text "High Disc.with rain"
    entry $f.dvh -width 5 -textvariable disvh_evap
    label $f.dvhu -text "(mm)"
    grid $f.lbl -row 0 -column 0 -sticky w
    grid $f.lb  -row 0 -column 1 -sticky w
    grid $f.lbu -row 0 -column 2 -sticky w
    grid $f.ubl -row 1 -column 0 -sticky w
    grid $f.ub  -row 1 -column 1 -sticky w
    grid $f.ubu  -row 1 -column 2 -sticky w
    grid $f.dl -row 2 -column 0 -sticky w
    grid $f.d  -row 2 -column 1 -sticky w
    grid $f.du  -row 2 -column 2 -sticky w
    grid $f.dhl -row 3 -column 0 -sticky w
    grid $f.dh  -row 3 -column 1 -sticky w
    grid $f.dhu  -row 3 -column 2 -sticky w
    grid $f.dvhl -row 4 -column 0 -sticky w
    grid $f.dvh  -row 4 -column 1 -sticky w
    grid $f.dvhu  -row 4 -column 2 -sticky w
    grid $tfe -in $root -row 3 -column 0 -padx 2 -pady 2 -sticky new

    grid columnconfigure $root 3 -weight 1
    grid rowconfigure $root 4 -weight 1

    return $root
}

proc m_ui {root} {
    if {$root == "."} {
	set base ""
    } else {
	set base $root
    }

    text $base.t -font courier -yscrollcommand "$base.sb set" \
	-width 60  -wrap none
    scrollbar $base.sb -command "$base.t yview"
    label $base.hdr -textvariable headertext -height 1 -font courier -anchor w
    text $base.msg  -height 5 -font fixed \
	-yscrollcommand "$base.msb set"  -width 60 -wrap none
    scrollbar $base.msb -command "$base.msg yview"
    grid $base.hdr -row 0 -column 0 -sticky w 
    grid $base.t -row 1 -column 0 -sticky nsew
    grid $base.sb -row 1 -column 1 -sticky nsw
    grid $base.msg -row 2 -column 0 -sticky new 
    grid $base.msb -row 2 -column 1 -sticky nsw

    frame $base.f 
    button $base.f.scan -text scan -command "process $root"
    button $base.f.next -text next -command "nextwarning $root"
    button $base.f.prev -text prev -command "prevwarning $root"
    grid $base.f.scan -row 0 -column 2
    grid $base.f.next -row 0 -column 1
    grid $base.f.prev -row 0 -column 0
    grid $base.f -row 3 -column 0
    grid columnconfigure $root 0 -weight 1
    grid rowconfigure $root 1 -weight 1
    return $root
}

proc h_ui {root} {
    global TAMETDIR
    if {$root == "."} {
	set base ""
    } else {
	set base $root
    }

    text $base.t -font courier -yscrollcommand "$base.sb set" \
	-width 60  -wrap word
    scrollbar $base.sb -command "$base.t yview"
    grid $base.t -row 1 -column 0 -sticky nsew
    grid $base.sb -row 1 -column 1 -sticky nsw

    grid columnconfigure $root 0 -weight 1
    grid rowconfigure $root 1 -weight 1

    set fp [open $TAMETDIR/errlist.txt r]
    $base.t insert end [read $fp]
    close $fp

    $base.t see 0.0

    return $root
}

proc message {root text} {
    if {$root == "."} {
	set base ""
    } else {
	set base $root
    }
    $base.msg delete 0.0 end
    $base.msg insert end $text
    $base.msg mark set insert 0.0
    $base.msg see insert
    update
}
proc readin {root f} {
    if {$root == "."} {
	set base ""
    } else {
	set base $root
    }
    set w $base.t
    $w delete 0.0 end
    message $root "Reading $f"
    set fp [open $f r]
    $w insert end [read $fp]
    close $fp
    $w mark set insert 0.0
    $w see insert
    message $root "Ready. \"Scan\" the file for warnings."
}


proc process {root} {
   global warnings; catch {unset warnings}
   if {$root == "."} {
		set base ""
   } else {
		set base $root
   }
   set w $base.t
   foreach t [$w tag names] {
		$w tag delete $t
   }

   $w tag configure interesting -foreground blue
   $w tag configure warning -background red

	global thisFile
	set ext [file extension $thisFile]
	if {[string tolower $ext]==".met"} {
	    process_met $root $w
	} elseif {[string tolower $ext]==".p51"} {
	    process_p51 $root $w
	} else {
       error "Dont know what a '$ext' file is. Use met or p51."
	}
}

proc process_met {root w} {
   global warnings
   global radn_do_extra

    zero_variables
    set header {}; set lat_seen 0
    set n [lindex [split [$w index end] "."] 0]
    for {set line 0} {$line < $n} {incr line} {
	if {$line%100 == 0} {message $root "Checking line $line of $n"}
	set buf [$w get $line.0 $line.end]
	regsub -all "!\[^\$\]*" $buf "" buf
	if {$header == {}} {
	    # no headers seen yet
	    if {[string first "\[" $buf]>= 0 ||
		[string match "!*" [string trim $buf]] ||
		[string trim $buf] == {}} {
		;# comment
	    } elseif {[string first "=" $buf] > 0} {
		set lhs [string trim [string tolower \
					  [lindex [split [join $buf] "="] 0]]]
		set rhs [lindex [split $buf "="] 1]
		if {$lhs == "latitude"} {
		    global latitude
		    scan $rhs "%f" latitude
		    $w tag add interesting $line.0 $line.end
		    set lat_seen 1
		}
	    } else {
		global headertext; set headertext $buf
		set header [join [string tolower $buf]]
		foreach z $header {global $z}
		$w tag add interesting $line.0 $line.end
		incr line ;# skip units		
	    }
	} else {
	    if {[catch {foreach $header $buf {break}} msg]} {
		set r "Parse error $msg"
	    } else {
		set r [daily_main]
		if {$radn_do_extra} {
		    append r [daily_radiation]
		}
	    }
	    if {$r != {}} {
		set warnings($line) $r 
		$w tag add warning $line.0 $line.end
		$w tag add w$line $line.0 $line.end
	    }
	}
    }

    global wind_found
    if {[lsearch $header wind] < 0} {
	set wind_found 0
    } else {
	set wind_found 1
    }

    $w mark set insert 0.0
    $w see insert
    set msg "Finished.\n"
    if {!$lat_seen} {
	append msg "NB. No \"latitude\" found.\n"
    }
    if {[llength [array names warnings]] > 0} {
	append msg "[llength [array names warnings]] warnings.\n"
    } else {
	append msg "No warnings.\n"
    }
    append msg "Use \"next\" and \"prev\" to navigate quickly."
    message $root $msg
}
##
#-18.25 127.78 evap shifted,   2011 OLD HALLS CREEK
#  date    jday  tmax  tmin  rain  evap   rad   vp  
# 18890101    1  38.0  25.0   7.7  10.4  23.0  24.0 
proc process_p51 {root w} {
   global warnings
   global radn_do_extra

   zero_variables

	global headertext
	set headertext [$w get 2.0 2.end]

   global latitude
   scan [$w get 1.0 1.7] "%f" latitude

	global year day maxt mint rain evap radn vp
   set n [lindex [split [$w index end] "."] 0]
   for {set line 3} {$line < $n} {incr line} {
      if {$line%100 == 0} {message $root "Checking line $line of $n"}
	   set buf [$w get $line.0 $line.end]
      
      if {[catch {
	      set year [string range $buf 1 4]
   	   set day  [string range $buf 10 13]
      	set maxt [string range $buf 14 19]
	      set mint [string range $buf 20 25]
      	set rain [string range $buf 26 31]
   	   set evap [string range $buf 32 37]
	      set radn [string range $buf 38 43]} msg]} {
 			set warnings($line) $msg 
			$w tag add warning $line.0 $line.end
			$w tag add w$line $line.0 $line.end
	   }

#   append msg "year='$year'"      
#   append msg "day ='$day'"      
#   append msg "maxt='$maxt'"      
#   append msg "mint='$mint'"      
#   append msg "rain='$rain'"      
#   append msg "radn='$radn'"      
#   append msg "evap='$evap'"      
#   message $root $msg
		set r [daily_main]
		if {$radn_do_extra} {
		    append r [daily_radiation]
		}
      if {$r != {}} {
 			set warnings($line) $r 
			$w tag add warning $line.0 $line.end
			$w tag add w$line $line.0 $line.end
	   }
   }

   global wind_found
   set wind_found 0

   $w mark set insert 0.0
   $w see insert
   set msg "Finished.\n"
   if {[llength [array names warnings]] > 0} {
		append msg "[llength [array names warnings]] warnings.\n"
   } else {
		append msg "No warnings.\n"
   }
   append msg "Use \"next\" and \"prev\" to navigate quickly."
   message $root $msg
}
##
proc nextwarning {root} {
    if {$root == "."} {
	set base ""
    } else {
	set base $root
    }
    set w $base.t

    global warnings
    set pos [lindex [$w tag nextrange warning [$w index insert+1l]] 0]
    if {$pos == {}} {
	set pos end
	message $root "At end of file"
    } else {
	set alltags [$w tag names $pos]
	foreach tag $alltags {
	    if {[scan $tag "w%d" wnum] == 1} {
		message $root $warnings($wnum)
	    }
	}
    }
    $w mark set insert $pos
    $w see $pos
}
proc prevwarning {root} {
    if {$root == "."} {
	set base ""
    } else {
	set base $root
    }
    set w $base.t

    global  warnings
    set pos [lindex [$w tag prevrange warning [$w index insert-1l]] 0]
    if {$pos == {}} {
	set pos 0.0
	message $root "At start of file"
    } else {
	set alltags [$w tag names $pos]
	foreach tag $alltags {
	    if {[scan $tag "w%d" wnum] == 1} {
		message $root $warnings($wnum)
	    }
	}
    }
    $w mark set insert $pos
    $w see $pos
}

proc OpenFile {root} {
    global thisFile
    set types {
        {{Apsim Met files}      {.met}        }
        {{GRASP p51 files}      {.p51}        }
        {{All Files}         *          }
    }
    set f [tk_getOpenFile -filetypes $types]
    if {$f != {}} {
	readin $root $f
	wm title [winfo toplevel $root] "Tamet - $f"
	set thisFile $f
    }
}
proc SaveAsFile {root} {
    global thisFile
    set types {
        {{Met files}      {.met}        }
        {{All Files}         *          }
    }
    set f [tk_getSaveFile -filetypes $types]
    if {$f != {}} {
	wm title [winfo toplevel $root] "Tamet - $f"
	set thisFile $f
	SaveFile $root
    }
}
proc SaveFile {root} {
    global thisFile
    if {$root == "."} {
	set base ""
    } else {
	set base $root
    }

    set fp [open $thisFile w]
    puts -nonewline $fp [$base.t get 0.0 end]
    close $fp
}
proc main_ui {root} {
    if {$root == "."} {
	set base ""
    } else {
	set base $root
    }

    set nb [NoteBook $base.nb]
    set m [$nb insert end scan -text "Check"]
    m_ui $m
    set p [$nb insert end param -text "Parameters"]
    p_ui $p
    set h [$nb insert end help -text "Notes"]
    h_ui $h

    $nb compute_size
    pack $base.nb -fill both -expand yes
    $nb raise [$nb page 0]

    [winfo toplevel $root] config -menu $base.menu
    
    menu $base.menu -tearoff "0"
    $base.menu add cascade  -label "File" -menu "$base.menu.file"
    $base.menu add cascade  -label "Help" -menu "$base.menu.help"

    menu $base.menu.file -tearoff "0"
    $base.menu.file add command -label "Open" -command "OpenFile $m"
    $base.menu.file add command -label "Save" -command "SaveFile $m"
    $base.menu.file add command -label "Save As" -command "SaveAsFile $m"
    $base.menu.file add separator
    $base.menu.file add command -label "Exit" -command exit

    menu $base.menu.help -tearoff "0"
    $base.menu.help add command -label "Ha Ha Ha"

    wm title [winfo toplevel $root] "Tamet - no file"
    message $m "Ready. Open a file through the menu."
}

foreach c [winfo chi .] {destroy $c}
main_ui .

# From Greg Mclean's "photoperiod.exe"
proc daily_radiation {} {
    global latitude day radn radn_cloud_low radn_cloud_high
    set pi 3.14159265359
    set rLatitude [expr {$latitude * ($pi/180.0)}]

    set SolarDec [expr {($pi/180.0) * 23.45 * sin(2*$pi*(284+$day)/365.0)}]
    set dayl [expr {acos(-tan($rLatitude) * tan($SolarDec))}]
    
    set So [expr {24.0 * 3600.0 * 1360.0 * \
		      ($dayl*sin($rLatitude)*sin($SolarDec)+ \
			   cos($rLatitude)*cos($SolarDec)*sin($dayl)) / \
		      ($pi*1000000.0)}]
    set sg_min [expr {$So * $radn_cloud_low}]
    set sg_max [expr {$So * $radn_cloud_high}]
    set result {}
    if {$radn < $sg_min} {
	append result "Radiation below [format %.2f $sg_min] Mj/ha"
    }
    if {$radn > $sg_max} {
	append result "Radiation above [format %.2f $sg_max] Mj/ha"
    }
    return $result
}
 
#readin . dalby.met
#readin .t /win/APSWork/hayman/041521.met
#process .t
