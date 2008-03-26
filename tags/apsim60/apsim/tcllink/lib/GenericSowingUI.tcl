#! ManagerX
trace remove variable XMLDoc read setXML

package require Tk

catch {unset config}
catch {destroy .w}
set w [frame .w]

package require BWidget
package require tdom
package require Img

# Data support procedures

# Return the apsim module this crop is an alias for (eg mungbean=plant, sorghum=sorghum etc)
proc cropModule {crop} {
   if {[lsearch [list \
    barley       \
    broccoli     \
    butterflypea \
    canola       \
    chickpea     \
    cowpea       \
    fababean     \
    fieldpea     \
    horsegram    \
    lablab       \
    lucerne      \
    lupin        \
    mucuna       \
    mungbean     \
    navybean     \
    oats         \
    peanut       \
    pigeonp      \
    soybean      \
    stylo        \
    SweetCorn    \
    SweetSorghum \
    weed         \
    wheat] $crop] >= 0} {return "plant"}

   if {[lsearch [list \
    bambatsi     \
    egrandis] $crop] >= 0} {return "growth"}

   if {[lsearch [list \
    maize        \
    sunflower] $crop] >= 0} {return "cropmod"}

   if { $crop == "cotton"} {return "ozcot"}
   if { $crop == "rice" } {return "oryza"}
   if { $crop == "sorghum" } {return "sorghum"}
   if { $crop == "sugar" } {return "sugar"}

   return $crop
}

# Return a list of crops that are plugged into this simulation
proc findCrops {} {
   global config
   set result {}
   set knownCrops {}
   foreach node [[$config(typesDoc) documentElement] childNodes] {
      foreach child [$node childNodes] {
         if {[string equal -nocase [$child nodeName] "IsCrop"]} {
             if {[string equal -nocase [$child text] "yes"]} {
                lappend knownCrops [$node nodeName]
             }
         }
      }
   }
   
   foreach crop $knownCrops {
      if {[llength [$config(gdocroot) selectNodes //$crop]] > 0} {
          lappend result $crop
      }
   }
   return $result
}

proc getCultivars {crop} { getTypesItem cultivar $crop }
proc getClasses {crop}   { getTypesItem class $crop }
proc getTypesItem {what crop} {
   global apsuite config

   foreach node [[$config(typesDoc) documentElement] childNodes] {
      if {[string equal -nocase [$node nodeName] "$crop"]} {
          set cvs {}
          foreach child [$node childNodes] {
             if {[string equal -nocase [$child nodeName] "$what"]} {
                 set name [$child getAttribute name]
                 if {[lsearch $cvs $name] < 0} {lappend cvs $name}
             }
          }
          return $cvs
      }
   }
   tk_messageBox -title "Error" -message "No ${what}s found for $crop" -type ok
   return ""
}

proc getImage {crop}     { 
   global apsuite config
   foreach node [[$config(typesDoc) documentElement] childNodes] {
      if {[string equal -nocase [$node nodeName] "$crop"]} {
          set img {}
          foreach child [$node childNodes] {
             if {[string equal -nocase [$child nodeName] "Image"]} {
                 set name [$child text]
                 regsub {%apsuite} $name $apsuite name
                 return $name
             }
          }
      }
   }
   return ""
}

proc addVariable {name} {
   global config
   if {![info exists config(variables)]} {set config(variables) $name}
   if {[lsearch $config(variables) $name] < 0} {lappend config(variables) $name}
}

##########UI procedures
proc CropUI  {w} {
   global config
   frame $w.c
   label $w.c.l -text "Crop"
   ComboBox $w.c.c -editable 0 -textvariable config(crop) \
                   -values $config(crops) \
                   -modifycmd "cropChanged $w" \
                   -helptext "Crop to sow" \
                   -width 10
   grid $w.c.l -row 1 -column 1
   grid $w.c.c -row 1 -column 2
   addVariable crop
   return $w.c
}

proc VariableDateWindowUI {w} {
   LabelFrame $w.d -text "Planting window & criteria" -side top  -bd 1
   set f [$w.d getframe]
   set row 1
   foreach {name text desc units} {
       date1 Start "Start of planting window" ""\
       date2 End "End of planting window" ""\
       raincrit Rainfall "Amount of rainfall required for planting" "(mm)" \
       rainnumdays "Rain days" "Number of days this rainfall occurs in" ""\
       esw_amount "Avail SW" "Minimum allowable available soil water for planting" "(mm)"} {
      label $f.${name}label -text $text
      Entry $f.${name}entry -textvariable config($name) -helptext $desc -width 8
      label $f.${name}units -text $units
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}entry -row $row -column 2
      grid $f.${name}units -row $row -column 3 -sticky w
      incr row
      addVariable $name
   }
   checkbutton $f.must_sow -text "Must Sow" -onvalue yes -offvalue no -variable config(must_sow)
   DynamicHelp::add $f.must_sow -text "Must sow crop at end of planting window"
   grid $f.must_sow -row $row -column 1 -columnspan 2
   addVariable must_sow

   grid $w.d -row 1 -column 1
   return $w.d
}

proc FixedDateWindowUI  {w} {
   LabelFrame $w.d -text "Fixed planting date"  -side top   -bd 1
   set f [$w.d getframe]
   label $f.d -text "Date"
   Entry $f.d1 -textvariable config(date1) -helptext "Planting date for all years"
   addVariable date1

   grid $f.d -row 1 -column 1
   grid $f.d1 -row 1 -column 2
   grid $w.d -row 1 -column 1
   return $w.d 
}

proc DateWindowUI  {w} {
   LabelFrame $w.t -text "Sowing Criteria" -side top   -bd 1
   set f [$w.t getframe]
   radiobutton $f.v -text "Variable" \
      -variable config(criteria) \
      -value    variable \
      -command  "dateChanged $w"
   DynamicHelp::add $f.v -text "Use variable sowing criteria"
   radiobutton $f.f -text "Fixed date" \
      -variable config(criteria) \
      -value    fixed \
      -command  "dateChanged $w"
   DynamicHelp::add $f.f -text "Use fixed sowing criteria"
   grid $f.v -row 1 -column 1
   grid $f.f -row 1 -column 2
   grid $w.t -row 1 -column 1
   addVariable criteria
   return $w.t 
}

proc dateChanged {w} {
   global config
   if {![info exists config(criteria)]} {set config(criteria) "variable"}
   if {$config(criteria) == "variable"} {
      catch {destroy $w.d}
      set r [VariableDateWindowUI $w]
   } elseif {$config(criteria) == "fixed"} {
      catch {destroy $w.d}
      set r [FixedDateWindowUI $w]
   } else {
      error "Sowing criteria $config(criteria) is unknown"
   }
   grid $r -row 3 -column 1 -sticky w -pady 3
}

proc CropParametersUI {w} {
   global config
   set module [cropModule $config(crop)]
   catch {destroy $w.cp}
   ${module}UI [frame $w.cp]
   grid $w.cp -row 4 -column 1 -sticky w -pady 3
   set filename [getImage $config(crop)]
   if {[file exists $filename]} {cropImage config -file $filename}

   addVariable module 
   set config(module) $module
}

################PLANT################
proc plantUI {w} {
   global config
   LabelFrame $w.t -text "Sowing Parameters (plant)" -side top   -bd 1
   set f [$w.t getframe]
   set row 1

   foreach {name text desc units} {
       plants        "Sowing Density" "Established sowing density"  "(plants/m2)" \
       sowing_depth  "Sowing Depth"   "Sowing depth" "(mm)" \
       row_spacing   "Row Spacing"    "Space between rows" "(mm)"} {
      label $f.${name}label -text $text
      Entry $f.${name}entry -textvariable config($name) -helptext $desc -width 12
      label $f.${name}units -text $units
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}entry -row $row -column 2 -sticky w
      grid $f.${name}units -row $row -column 3 -sticky w
      addVariable $name
      incr row
   }

   foreach {name text desc} {skipplant  "Skip plant" "Skip plant factor" \
                             skiprow    "Skip row"   "Skip row factor" } {
      label $f.${name}label -text $text
      ComboBox $f.${name}cbx -editable 0 \
                   -textvariable config($name) \
                   -values  [list 0 1 2]\
                   -helptext $desc  -width 3
      $f.${name}cbx setvalue @0
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}cbx -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }                                 

   foreach {name text desc what} {cultivar  "Cultivar"   "Cultivar of crop" "cultivar" \
                                  class     "Crop class" "Crop class"       "class"} {
      set whats [getTypesItem $what $config(crop)]
      if {[llength $whats] > 1} {
         label $f.${name}label -text $text
         ComboBox $f.${name}cbx -editable 0 \
                      -textvariable config($name) \
                      -values  $whats\
                      -helptext $desc -width 12
         set index [lsearch $whats $config($name)]
         if {$index < 0} {set index 0}
         $f.${name}cbx setvalue @$index
         grid $f.${name}label  -row $row -column 1 -sticky e
         grid $f.${name}cbx -row $row -column 2 -sticky w
         incr row
      } else {
         set config($name) [lindex $whats 0]
      }   
      addVariable $name
   }                                 

   grid $w.t -row 1 -column 1
   return $w.t 
}

################SORGHUM################
proc sorghumUI {w} {
   global config
   LabelFrame $w.t -text "Sowing Parameters (sorghum)" -side top   -bd 1
   set f [$w.t getframe]
   set row 1

   foreach {name text desc units} {
        plants             "Sowing Density"  "Sowing density"  "(plants/m2)" \
        sowing_depth       "Sowing Depth"    "Sowing depth"    "(mm)" \
        row_spacing        "Row Spacing"     "Row spacing"     "(m)"
        tiller_no_fertile  "Fertile tillers" "Number of fertile tillers" ""} {
      label $f.${name}label -text $text
      Entry $f.${name}entry -textvariable config($name) -helptext $desc -width 12
      label $f.${name}units -text $units
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}entry -row $row -column 2 -sticky w
      grid $f.${name}units -row $row -column 3 -sticky w
      addVariable $name
      incr row
   }

   foreach {name text desc} {skiprow    "Skip row"   "Skip row configuration" } {
      label $f.${name}label -text $text
      ComboBox $f.${name}cbx -editable 0 \
                   -textvariable config($name) \
                   -values  [list solid single double]\
                   -helptext $desc -width 5
      $f.${name}cbx setvalue @0
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}cbx -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }                                 

   foreach {name text desc what} {cultivar  "Cultivar"   "Cultivar of crop" "cultivar"} {
      label $f.${name}label -text $text
      set whats [getTypesItem $what $config(crop)]
      ComboBox $f.${name}cbx -editable 0 \
                   -textvariable config($name) \
                   -values  $whats\
                   -helptext $desc  -width 12
      set index [lsearch $whats $config($name)]
      if {$index < 0} {set index 0}
      $f.${name}cbx setvalue @$index
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}cbx -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }                                 

   grid $w.t -row 1 -column 1
   return $w.t 
}
################CROPMOD################
proc cropmodUI {w} {
   global config
   LabelFrame $w.t -text "Sowing Parameters (cropmod)" -side top   -bd 1
   set f [$w.t getframe]
   set row 1

   foreach {name text desc units} {
        plants             "Sowing Density"  "Sowing density"  "(plants/m2)" \
        sowing_depth       "Sowing Depth"    "Sowing depth"    "(mm)" \
        row_spacing        "Row Spacing"     "Row spacing"     "(m)"} {
      label $f.${name}label -text $text
      Entry $f.${name}entry -textvariable config($name) -helptext $desc -width 12
      label $f.${name}units -text $units
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}entry -row $row -column 2 -sticky w
      grid $f.${name}units -row $row -column 3 -sticky w
      addVariable $name
      incr row
   }

   foreach {name text desc} {skiprow    "Skip row"   "Skip row configuration" } {
      label $f.${name}label -text $text
      ComboBox $f.${name}cbx -editable 0 \
                   -textvariable config($name) \
                   -values  [list solid single double]\
                   -helptext $desc -width 5
      $f.${name}cbx setvalue @0
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}cbx -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }                                 

   foreach {name text desc what} {cultivar  "Cultivar"   "Cultivar of crop" "cultivar"} {
      label $f.${name}label -text $text
      set whats [getTypesItem $what $config(crop)]
      ComboBox $f.${name}cbx -editable 0 \
                   -textvariable config($name) \
                   -values  $whats\
                   -helptext $desc  -width 12
      set index [lsearch $whats $config($name)]
      if {$index < 0} {set index 0}
      $f.${name}cbx setvalue @$index
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}cbx -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }                                 

   grid $w.t -row 1 -column 1
   return $w.t 
}
################OZCOT################
proc ozcotUI {w} {
   global config
   LabelFrame $w.t -text "Sowing Parameters (cotton)" -side top   -bd 1
   set f [$w.t getframe]
   set row 1

   foreach {name text desc units} {
        plants_pm          "Sowing Density"  "Sowing density"  "(plants/m in row)" \
        sowing_depth       "Sowing Depth"    "Sowing depth"    "(mm)" \
        row_spacing        "Row Spacing"     "Row spacing"     "(mm)"} {
      label $f.${name}label -text $text
      Entry $f.${name}entry -textvariable config($name) -helptext $desc -width 12
      label $f.${name}units -text $units
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}entry -row $row -column 2 -sticky w
      grid $f.${name}units -row $row -column 3 -sticky w
      addVariable $name
      incr row
   }

   foreach {name text desc} {skiprow    "Skip row"   "Skip row configuration" } {
      label $f.${name}label -text $text
      ComboBox $f.${name}cbx -editable 0 \
                   -textvariable config($name) \
                   -values  [list 0 1 2]\
                   -helptext $desc -width 5
      $f.${name}cbx setvalue @0
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}cbx -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }                                 

   foreach {name text desc what} {cultivar  "Cultivar"   "Cultivar of crop" "cultivar"} {
      label $f.${name}label -text $text
      set whats [getTypesItem $what $config(crop)]
      ComboBox $f.${name}cbx -editable 0 \
                   -textvariable config($name) \
                   -values  $whats\
                   -helptext $desc  -width 12
      set index [lsearch $whats $config($name)]
      if {$index < 0} {set index 0}
      $f.${name}cbx setvalue @$index
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}cbx -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }                                 

   grid $w.t -row 1 -column 1
   return $w.t 
}

################ORYZA################
proc oryzaUI {w} {
   global config
   LabelFrame $w.t -text "Sowing Parameters (oryza)" -side top   -bd 1
   set f [$w.t getframe]
   set row 1

   foreach {name text desc what} {cultivar  "Cultivar"   "Cultivar of crop" "cultivar"} {
      label $f.${name}label -text $text
      set whats [getTypesItem $what $config(crop)]
      ComboBox $f.${name}cbx -editable 0 \
                   -textvariable config($name) \
                   -values  $whats\
                   -helptext $desc  -width 12
      set index [lsearch $whats $config($name)]
      if {$index < 0} {set index 0}
      $f.${name}cbx setvalue @$index
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}cbx -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }                                 

   grid $w.t -row 1 -column 1
   return $w.t 
}
################OZCOT################
proc sugarUI {w} {
   global config
   LabelFrame $w.t -text "Planting Parameters (sugar)" -side top   -bd 1
   set f [$w.t getframe]
   set row 1

   foreach {name text desc units} {
        plants             "Sowing Density"  "Sowing density"  "(plants/m2)" \
        sowing_depth       "Sowing Depth"    "Sowing depth"    "(mm)"} {
      label $f.${name}label -text $text
      Entry $f.${name}entry -textvariable config($name) -helptext $desc -width 12
      label $f.${name}units -text $units
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}entry -row $row -column 2 -sticky w
      grid $f.${name}units -row $row -column 3 -sticky w
      addVariable $name
      incr row
   }

   foreach {name text desc what} {cultivar  "Cultivar"   "Cultivar of crop" "cultivar"} {
      label $f.${name}label -text $text
      set whats [getTypesItem $what $config(crop)]
      ComboBox $f.${name}cbx -editable 0 \
                   -textvariable config($name) \
                   -values  $whats\
                   -helptext $desc  -width 12
      set index [lsearch $whats $config($name)]
      if {$index < 0} {set index 0}
      $f.${name}cbx setvalue @$index
      grid $f.${name}label  -row $row -column 1 -sticky e
      grid $f.${name}cbx -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }                                 

   grid $w.t -row 1 -column 1
   return $w.t 
}
################..............

proc cropChanged {w} { CropParametersUI $w }

proc editRule {node w} {
   if {[winfo exists $w.edit]} {destroy $w.edit}
   set f [frame $w.edit]
   ScrolledWindow $f.sw
   text $f.sw.t -height 30 -wrap none -font courier
   $f.sw setwidget $f.sw.t
   $f.sw.t insert end [$node text]
   Button $f.ok -text "Ok" -command "finishEditRule $node $w.edit $f.sw.t"
   Button $f.can -text "Cancel" -command "destroy $w.edit"
   pack $f.sw -fill both -expand 1
   pack $f.ok $f.can -side left -padx 10
   grid $w.edit -row 1 -column 3 -rowspan 6 -columnspan 2  -sticky ne
   focus $f.sw.t

   $f.sw.t tag configure variables -foreground blue
   $f.sw.t tag configure subst     -foreground red -background gray

   markVariables $f.sw.t

   global config
   $f.sw.t insert end "!Current variables: (not saved - do not edit)\n" variables
   foreach v $config(variables) {
      $f.sw.t insert end "!$v=$config($v)\n" variables
   }
   bind $f.sw.t <Any-KeyRelease> "+markVariables $f.sw.t"
}

proc markVariables {t}  {
   global config
   foreach {a b} [$t tag ranges subst] {$t tag remove subst $a $b}
   set lc 1
   foreach line [split [$t get 0.0 end] "\n"] {
      foreach v $config(variables) {
         if {[set s [string first "\[$v\]" $line]] >= 0} {
             $t tag add subst $lc.$s $lc.[expr $s+[string length $v]+2]
         }
      }
      incr lc
   }
}

proc finishEditRule {node w t} {
    global config
    set new [$config(doc) createElement ruletemplate]
    foreach child [$node childNodes] {$child delete}
    foreach {a b} [$t tag range variables] {$t delete $a $b}
    $node appendChild [$config(doc) createTextNode [$t get 0.0 end]]
    destroy $w
}

proc additionalUI {w} {
   global config

   LabelFrame $w.p -text "Additional parameters" -side top -bd 1
   set f [$w.p getframe]
   set row 1

   foreach node [$config(docroot) childNodes] {
      set nodeName [$node nodeName]
      if {$nodeName != "uiscript" && 
          $nodeName != "rule" &&
          $nodeName != "ruletemplate" &&
          [lsearch $config(variables) $nodeName] < 0} {
         label $f.r${row}label -text $nodeName
         entry $f.r${row}entry -textvariable config($nodeName)

         grid $f.r${row}label -row $row -column 1 -sticky e
         grid $f.r${row}entry -row $row -column 2 -sticky w
         addVariable $nodeName
         incr row
      }
   }

   Button $f.r$row -text "Add parameter" -command "addParameter $w $f $row" -helptext "Add an editable parameter to this UI" 
   grid $f.r$row -row $row -column 1 -sticky w -pady 5  -padx 5
   return $w.p
}

proc addParameter {w f row} {
   destroy $f.r${row}

   entry $f.r${row}entry -textvariable config(additional)
   grid $f.r${row}entry -row $row -column 1 -sticky e

   bind $f.r${row}entry <Key-Return> "acceptNewParameter $w $f $row"
}

proc acceptNewParameter {w f row} {
   global config
   destroy $f.r${row}entry

   label $f.r${row}label -text $config(additional)
   entry $f.r${row}entry -textvariable config($config(additional))
   lappend config(variables) $config(additional)
   grid $f.r${row}label -row $row -column 1 -sticky e
   grid $f.r${row}entry -row $row -column 2 -sticky w

   incr row
   unset config(additional)
   Button $f.r$row -text "Add parameter" -command "addParameter $w $f $row" -helptext "Add an editable parameter to this UI" 
   grid $f.r$row -row $row -column 1 -sticky w -pady 5  -padx 5
}

proc editRulesUI {w} {
   global config

   LabelFrame $w.r -text "Rules" -side top -bd 1
   set f [$w.r getframe]
   set row 1
   grid $w.r -row 1 -column 1

   foreach ruleNode $config(rules) {
      Button $f.r$row -text [$ruleNode getAttribute name] -command "editRule $ruleNode $w" -helptext "Edit this rule" 
      grid $f.r$row -row $row -column 1 -sticky w -columnspan 2 -pady 5
      incr row
   }

#   Button $f.r$row -text "Test" -command "setXML junk junk junk" 
#   grid $f.r$row -row $row -column 1 -sticky w -columnspan 2 -pady 5
#   incr row
   return $w.r 
}

proc MainUI  {w} {
   global config
   image create photo cropImage
   grid [CropUI $w] -row 1 -column 1 -sticky w  -pady 3
   grid [DateWindowUI  $w] -row 2 -column 1 -sticky w  -pady 3
   dateChanged $w
   cropChanged $w
   grid [additionalUI  $w] -row 5 -column 1 -sticky w  -pady 3
   grid [editRulesUI  $w] -row 6 -column 1 -sticky w  -pady 3
   
   label .w.img -image cropImage
   grid .w.img -row 1 -column 3 -rowspan 6 -columnspan 2  -sticky ne
   
   grid columnconfigure $w 2 -weight 1
   grid rowconfigure    $w 7 -weight 1
   return $w
}

###################Code starts here#########

## Decode the XML string for this applet
set config(doc)      [dom parse $XMLDoc]
set config(docroot)  [$config(doc) documentElement]
set config(gdocroot) [[dom parse $GlobalXMLDoc] documentElement]

set fp [open $apsuite/APSIMUI/Types.xml r]
set config(typesDoc) [dom parse [read $fp]]
close $fp

set config(crops) [findCrops] 
set config(rules) {}
foreach node [$config(docroot) childNodes] {
   if {[$node nodeName] != "ruletemplate"} {
      set config([$node nodeName]) [$node text]
   } else {
      lappend config(rules) $node
   }
}
addVariable uiscript

proc processRule {node} {
   global config
   set template [$node text]
   foreach v $config(variables) {
      regsub -all "\\\[$v\\\]" $template $config($v) template
   }
   return $template
}

proc setXML {name1 name2 op} {
   global XMLDoc config myName 
   catch {
      set newDoc [dom createDocument [$config(docroot) nodeName]]
      set root [$newDoc documentElement]
      $root setAttribute name [$config(docroot) getAttribute name]
      foreach rule $config(rules) {
         set new [$newDoc createElement rule]
         $new appendChild [$newDoc createTextNode [processRule $rule]]
         $new setAttribute name      "$myName - [$rule getAttribute name]"
         $new setAttribute condition [$rule getAttribute condition]
         $new setAttribute invisible yes
         $root appendChild $new

         set new [$newDoc createElement ruletemplate]
         $new setAttribute name [$rule getAttribute name]
         $new setAttribute condition [$rule getAttribute condition]
         $new appendChild [$newDoc createTextNode [$rule text]]
         $root appendChild $new
      }
      foreach variable $config(variables) {
         set new [$newDoc createElement $variable]
         $new appendChild [$newDoc createTextNode $config($variable)]
         $root appendChild $new
      }
   } msg
   if {$msg != ""} {
      tk_messageBox -title "Error" -message $msg -type ok
   } else {
      set XMLDoc [$newDoc asXML]
#      set fp [open c:/tmp/z.xml w]
#      puts $fp [$newDoc asXML]
#      close $fp
   }   
}

grid forget .
grid [MainUI $w] -row 0 -column 0 -sticky nwe
grid columnconf . 0 -weight 1
grid rowconf    . 0 -weight 1

trace add variable XMLDoc read setXML
