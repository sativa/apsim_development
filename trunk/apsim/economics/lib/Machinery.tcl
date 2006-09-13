# Apsim Machinery (sub)Module
# Contains Tractors, Implements & Labour.

# Operations: 
# bool available(<config>)    ; See if a configuration can be used today (read)
# operate <config> <area>     ; Operate a configuration over an area until finished (event)

# <config> is:
#  tractor=<name>,implement=<name>

#########################################################################
# support procedures. 
package require tdom

########################## Apsim interface code here
source [file dirname $tcl_library]/apsimDatatypes.tcl

# A trace handler for our 'fake' array. This handler is called whenever another module 
# asks us whether machinery is available.
#    NB. Careful! Errors in trace routines are not caught!!!
proc machinery:maProc {name1 name2 op} {
   global $name1
   if {$op == "read" && [string length $name2] > 0} {
      array set $name1 [list $name2 0]                   ;# off by default
      foreach nv [split $name2 ","] {
         foreach {name value} [split $nv "="] {
            regsub -all "'" $name {} name
            regsub -all "'" $value {} value
            set $name $value
         }
      }
      # Do stuff with args to determine what the value should be..
      if {![info exists tractor] || ![info exists implement]} {
         set msg "You must specify a tractor and implement operate with. (got $name2)"
         apsimWriteToSummaryFile $msg; error $msg
      }
      
      # Check they are known to us
      if {[catch {set tid [getTractorId $tractor]; set iid [getImplementId $implement]}]} {
         global errorInfo
         apsimWriteToSummaryFile "$msg\n$errorInfo"; error $msg
      }
      
      # Now see if they are busy
      set avail 1
      catch {
         global machinery:jobs
         foreach job ${machinery:jobs} {
            foreach {tid iid area} [split $job ","] {break}
            if {$tractor == [getName $tid] || $implement == [getName $iid]} {set avail 0}
         }
      } msg
      if {$msg != ""} {apsimWriteToSummaryFile $msg; error $msg}
           
      # Set the variable - this is what is returned to apsim.
      array set $name1 [list $name2 $avail]
   }  
}

# An event handler. 
proc machinery:operateHandler {args} {
  global incomingApsimVariant
  foreach {name value} [unpack_Variant $incomingApsimVariant] {
     regsub -all "'" $name {} name
     regsub -all "'" $value {} value
     set [string tolower $name] [string tolower $value]
  }
  if {![info exists tractor] || ![info exists implement] || ![info exists area]} {
     error "Must specify a tractor, implement, and area to operate over."
  }
  machinery:operate $tractor $implement $area
}

# The trace variable we use to trigger events when the variable is read
array set available {}
trace add variable available {array read} "machinery:maProc"

# Now tell apsim we own it (unnecessary)
apsimRegisterGetSet available

# Now register the event handlers. 
apsimRegisterEvent operate            "machinery:operateHandler"
apsimRegisterEvent process            "machinery:process"
apsimRegisterEvent end_financial_year "machinery:end_year"
########################## End apsim interface code

########################## Machinery configuration code
# All machinery has an "id" which is just a node in the XML tree.
# Read in our configuration to an XML tree
proc machinery:initialise {} {
   global config
   set config(xmldoc) [dom parse [apsimGetComponentXML]]
   set config(docroot) [$config(xmldoc) documentElement]
} 

# Get the value of an objects 'thing'
proc getValue {id thing} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         return [string tolower [$node text]]
      }
   }
   error "Object $id doesn't have a $thing node"
}

# Set the value of an objects 'thing'
proc setValue {id thing what} {
   global config
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         $id removeChild $node
         set node [$config(xmldoc) createElement $thing]
         $node appendChild [$config(xmldoc) createTextNode $what]
         $id appendChild $node
      }
   }
}

# Return the name of an object, given id.
proc getName {id} {
   return [getValue $id "name"]
}

# Return a list of tractors we know about
proc getTractorIds {} {
   global config
   return [$config(docroot) selectNodes //tractor]
}

proc getTractorNames {} {
   set result {}
   foreach id [getTractorIds] {lappend result [string tolower [getName $id]]}
   return $result
}

# Return the ID of a tractor we know about
proc getTractorId {name} {
   global config
   foreach fn [$config(docroot) selectNodes //tractor/name] {
      if {[string compare -nocase "[$fn text]" "$name"] == 0} {return [$fn parent]}
   }
   error "Tractor $name isn't in database (should be one of \"[join [getTractorNames] \",\"]\")"
}
proc getImplementIds {} {
   global config
   return [$config(docroot) selectNodes //implement]
}
proc getImplementNames {} {
   set result {}
   foreach id [getImplementIds] {lappend result [string tolower [getName $id]]}
   return $result
}
proc getImplementId {name} {
   global config
   foreach fn [$config(docroot) selectNodes //implement/name] { 
      if {[string compare -nocase "[$fn text]" "$name"] == 0} {return [$fn parent]}
   }
   error "Implement $name isn't in database (should be one of \"[join [getImplementNames] \",\"]\")"
}

# return the work rate this combo can cover per hour
proc getRate {tid iid} {
   set implement [getName $iid]
   foreach rn [$tid selectNodes workrate] {
       if {[string compare -nocase [$rn getAttribute implement] "$implement"] == 0} {
          return [$rn text]
       }
   }
   error "No work rate for [[$tid child 1] text] + [[$iid child 1] text] specified"
}

# return the fuel cost this combo uses per hour
proc getCost {tid iid} {
   set implement [getName $iid]
   foreach rn [$tid selectNodes fuelrate] {
       if {[string compare -nocase [$rn getAttribute implement] "$implement"] == 0} {
          set fuelrate [$rn text]
          return $fuelrate
       }
   }
   error "No fuel rate for [getName $tid] + [getName $iid] specified"
}

# Return the number of hours worked per day
proc getHoursPerDay {} {
   #global config
   #foreach fn [$config(docroot) selectNodes //hoursperday] {
   #   return [$fn text]
   #}
   #error "No hours per day defined"
   return 8.0
}

# Operate a configuration over an area. Just add it to the job queue 
# and let process look after it.
proc machinery:operate {tractorName implementName area} {
   set tid [getTractorId $tractorName]
   set iid [getImplementId $implementName]
   if {![string is double -strict $area]} {
      error "Area should be a number (not $area)"
   }
   global machinery:jobs
   lappend machinery:jobs $tid,$iid,$area
   apsimWriteToSummaryFile "Machinery job '[getName $tid] + [getName $iid]' is queued"
}

# The daily process routine. Manages the job queue
proc machinery:process {} {
   global machinery:jobs
   set tomorrowsJobs {}
   # Go through each job. If an item is in use in any prior job, we can't do it today. 
   # Ignores partial days.
   for {set ijob 0} {$ijob < [llength ${machinery:jobs}]} {incr ijob} {
      set job [lindex ${machinery:jobs} $ijob]
      foreach {tid iid area} [split $job ","] {break}
      set inuse 0
      for {set j 0} {$j < $ijob} {incr j} {
         foreach {Ttid Tiid Tarea} [split [lindex ${machinery:jobs} $j] ","] {break}
         if {$tid == $Ttid || $iid == $Tiid} {set inuse 1}
      }
      if {!$inuse} {
        set cost [expr [getHoursPerDay] * [getCost $tid $iid]]
        apsimSendMessage "" expenditure [list cost $cost] [list comment "operating costs of [getName $tid] + [getName $iid]"]

        set rate [expr [getHoursPerDay] * [getRate $tid $iid]]
        set area [expr $area - $rate]
        if {$area > 0} {
           lappend tomorrowsJobs $tid,$iid,$area
        } else {
           apsimWriteToSummaryFile "Machinery job '[getName $tid] + [getName $iid]' has finished"
        }
      } else {
        lappend tomorrowsJobs $job
      } 
   }
   set machinery:jobs $tomorrowsJobs
}

# The "end_year" routine. Do replacement costs.
proc machinery:end_year {} {
   foreach id [concat [getTractorIds] [getImplementIds]] {   
      set age [expr [getValue $id age] + 1]
      setValue $id age $age
      
      if {$age >= [getValue $id YearsToReplacement]} {
         set what [getName $id]
         set salvage [getValue $id SalvageValue]
         apsimSendMessage "" income [list amount $salvage] [list comment "salvage value of $what"]

         set cost [getValue $id MarketValue]
         apsimSendMessage "" expenditure [list cost $cost] [list comment "replacement of $what"]
         setValue $id age 0
      }   
   }  
}

# Finally, load our configuration database and initialise states
machinery:initialise 
set machinery:jobs {}
apsimWriteToSummaryFile "Machinery:\nTractors=[getTractorNames]\nImplements=[getImplementNames]"
