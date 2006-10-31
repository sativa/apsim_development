#! ManagerX
#foreach c [winfo children .] {destroy $c}
if {[winfo exists .w.nb]} {destroy .w.nb}
if {[winfo exists .w]} {destroy .w}

trace remove variable XMLDoc read setXML
set w [frame .w]

package require BWidget
package require tdom

proc propertiesUI  {w} {
   button $w.b -command "boing" -text Boing
   grid   $w.b -row 0 -column 0 -sticky nsew
}

## Decode the XML string for this applet
set doc [dom parse $XMLDoc]
set docroot [$doc documentElement]
set nb [NoteBook::create $w.nb]
propertiesUI [$nb insert 0 properties -text "Properties"]

foreach node [$docroot selectNodes //rule] {
   set name [$node getAttribute name]
   set f [$nb insert end $node -text $name]
   
   text $f.txt -height 25 -width 70 -yscrollcommand "$f.s set"
   scrollbar $f.s -orient v -command "$f.txt yview"
   grid $f.txt -in $f -row 0 -column 0 -sticky nsew
   grid $f.s -in $f -row 0 -column 1 -sticky nsw
   grid rowconf $f 0 -weight 1
   grid columnconf $f 0 -weight 1
   $f.txt insert end [$node text]
}

$nb compute_size
grid $w.nb -row 0 -column 0 -sticky nsew
grid rowconf    $w 0 -weight 1
grid columnconf $w 0 -weight 1
$nb raise [$nb page 0]

trace add variable XMLDoc read setXML
proc setXML {name1 name2 op} {
   global XMLDoc doc docroot 
   catch {
     foreach var {} {
        set new [$doc createElement $var]
        $new appendChild [$doc createTextNode [set $var]]
        set old [$docroot selectNodes //$var] 
        [$old parentNode] appendChild $new
        $old delete
     }
   } msg
   if {$msg != ""} {tk_messageBox -title "Error" -message $msg -type ok}      
   #set XMLDoc [$doc asXML]
}

grid $w -row 0 -column 0 -sticky nsew
grid rowconf    . 0 -weight 1
grid columnconf . 0 -weight 1
