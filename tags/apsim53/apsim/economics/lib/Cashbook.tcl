# Apsim/Economics Cashbook (sub)module

# accepts messages:
#  cashbook income {amount 64000.0} {comment "description here"}
#  cashbook income {category cropprice} {name wheat} {yield 4000} {protein 12.3} {comment "description here"}
#  cashbook income {category cropprice} {name sorghum} {yield 6000} {comment "description here"}
#  cashbook expenditure {cost 42.0} {comment "description here"}
#  cashbook expenditure {category seed} {name wheat} {rate 120} {comment "description here"}
# Keeps a balance, writes a log.

########################## Apsim interface code here
source [file dirname $tcl_library]/apsimDatatypes.tcl

apsimRegisterGetSet balance
apsimRegisterEvent  income      "cashbook:incomeHandler"
apsimRegisterEvent  expenditure "cashbook:expenditureHandler"
apsimRegisterEvent  process     "cashbook:processHandler"

# An event handler. 
#  cashbook income {amount 64000.0}                                              {comment "description here"}
#  cashbook income {category cropprice} {name wheat} {yield 4000} {protein 12.3} {comment "description here"}
#  cashbook income {category cropprice} {name sorghum} {yield 6000}              {comment "description here"}
proc cashbook:incomeHandler {args} {
   global incomingApsimVariant balance

   set names {}
   foreach {_name _value} [unpack_Variant $incomingApsimVariant] {
      set $_name $_value
      lappend names $_name
   }
   if {![info exists comment]} { set comment "" }
   
   if {[info exists amount] && [string is double -strict $amount]} {
      set balance [expr $balance + $amount]
   
   } elseif {[info exists category] && [info exists name] && [info exists yield] && [info exists area]} {
      set amount "NA"
      set price "NA"
      global docroot
      foreach node [$docroot selectNodes //$category] {
         if {[string equal -nocase [getValue $node name] $name]} {
             if {$name != "wheat"} {
                set price [getValue $node price]
             } else {
                # oddball wheat pricing system..
                set proteins [getValue $node protein]
                set prices [getValue $node price]
                set price [lindex $prices 0]
                for {set i 0} {($i < [llength $proteins]) && 
                               ($protein > [lindex $proteins $i])} {incr i} {
                   set price [lindex $prices $i]
                }
             }  
             set amount [expr  $price * $yield * $area]
         }
      }
      set comment "$category ($name)"
      apsimWriteToSummaryFile "Priced $name $category (price='$price', yield='$yield') over '$area' ha = '$amount'"
      set balance [expr $balance + $amount]
   } else {
      error "cashbook:income: Must specify a either an numeric amount, or <category, name, price and area>."
   }

   cashbook:log income $amount $comment
}

proc cashbook:expenditureHandler {args} {
  global incomingApsimVariant
  foreach {_name _value} [unpack_Variant $incomingApsimVariant] {
     set $_name $_value
  }
  if {![info exists comment]} { set comment "" }

  if {[info exists cost]} {
     # Simple 
     global balance
     set balance [expr $balance - $cost]
  } elseif {[info exists category] && [info exists name] && [info exists rate] && [info exists area]} {
     # lookup this expense in our table of costs

     set cost "NA"
     global docroot
     foreach node [$docroot selectNodes //$category] {
        if {[string equal -nocase [getValue $node name] $name]} {
            set price [getValue $node price]
            set cost [expr $price * $rate * $area]
        }
     }
     if {![string is double -strict $cost]} {error "No price info for $category/$name"}

     set comment "$category ($name)"
     apsimWriteToSummaryFile "Costed $name $category (rate=$rate, price=$price) over $area ha = $cost"
     
     global balance
     set balance [expr $balance - $cost]
  } else {
     error "cashbook:expenditure: Must specify a either a cost or a (category + name + rate + area)."
  }

  cashbook:log expenditure $cost $comment
}

# Add up the annual farm overheads
proc cashbook:doFarmOverheads {} {
   global docroot balance
   set sum 0.0
   foreach node [$docroot selectNodes //overhead] {
      set sum [expr $sum + [getValue $node value]]
   }
      
   set balance [expr $balance - $sum]
   cashbook:log expenditure $sum "Farm Overheads"
}

# Work our repayments on initial investment
proc cashbook:doInitialCapital {} {
   global docroot balance
   set sum 0.0

# Set the period of initial loan
setValue [lindex [$docroot selectNodes //icapital] 0] loanPeriod 1

   set node [lindex [$docroot selectNodes //icapital] 0]
   if {[getValue $node loanPeriod] <=  [getValue $node loanDuration]} {
      #A = P(i(1+i)^n)/((1+i)^n - 1)
      set P [getValue $node ivalue] 
      set i [expr [getValue $node loanRate]/100.0]
      set n [getValue $node loanDuration]
      
      set A [expr $P * ($i*pow(1+$i,$n))/(pow(1.0+$i,$n) - 1.0) ]

      set balance [expr $balance - $A]
      cashbook:log expenditure $A "Loan repayments for initial capital outlay"
      
      setValue $node loanPeriod [expr 1 + [getValue $node loanPeriod]]
      if { [getValue $node loanPeriod] >  [getValue $node loanDuration] } {
          apsimWriteToSummaryFile "Loan for initial capital outlay is finished"
      }    
   }
}
     


# Send an "end financial year" message when needed
proc cashbook:processHandler {args} {
  if {[apsimGet day] == 181} {
     cashbook:doFarmOverheads
     cashbook:doInitialCapital
     apsimSendMessage "" end_financial_year
  }
}

proc cashbook:log {what amount comment} {
  global balance cashbook:outputfilename
  set fp [open ${cashbook:outputfilename} a]
  if {$what == "income"} {
     puts $fp "[apsimGet dd/mmm/yyyy],$amount,,$balance,$comment"
  } elseif {$what == "expenditure"} {
     puts $fp "[apsimGet dd/mmm/yyyy],,$amount,$balance,$comment"
  } else {
     error "Unknown cashbook operation '$what'"
  }
  close $fp
}


########################## End apsim interface code
## Read our initial conditions

package require tdom
set doc [dom parse [apsimGetComponentXML]]
set docroot [$doc documentElement]

set balance [[$docroot selectNodes //balance] text]
set cashbook:outputfilename [[$docroot selectNodes //outputfilename] text]

set fp [open ${cashbook:outputfilename} w]
puts $fp "date,income,expenditure,balance,comment"
close $fp