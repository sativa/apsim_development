# Condor postbox

# Set up the postbox area (this should be a network share)
set postbox d:/CondorPostbox
catch {[file mkdir $postbox]}

# The array of submit files we know about:
array set mySubs {}

# (Recursively) scan a directory for submit files
proc scanDir {dir} {
   foreach f [glob -nocomplain -dir $dir *] {
      if {[file isdirectory $f]} {
        scanDir $f
      } elseif {[file extension $f] == ".sub"} {
        checkSub $f
      }
   }
}

# Check a submit file to see whether action needs to happen
proc checkSub {f} {
   global mySubs
   set mtime [lindex [array get mySubs $f] 1]
   if {$mtime == {}} {
      # This is a new one we don't know about
      submit $f
   } elseif {[file mtime $f] > $mtime} {
      # It has changed since we last looked (resubmitted?)
      submit $f
   } else {
      # Is in the system.
   }
}

# Submit a submit file to the condor system
proc submit {f} {
   global mySubs
   cd [file dirname $f]
   if {[catch {exec condor_submit $f} msg]} {
     set fp [open CondorPostbox.SubmitError.txt a]
     puts $fp [clock format [clock seconds]]]
     puts $fp $msg
     close $fp
   } 
   # Keep a record of this file so we don't try and continually resubmit
   set mySubs($f) [file mtime $f]
}

# If a submit file disappears, then remove it from out list
proc cleanUp {} {
   global mySubs
   foreach f [array names mySubs *] {
      if {![file exists $f]} {
         array unset mySubs $f
      }
   }
}

# Loop forever
while {1} {
   scanDir $postbox
   cleanUp 
   after 5000
}
exit
