set apsuite c:/development
set fp [open "C:/Documents and Settings/devoilp/My Documents/zzz.apsim" r]
set GlobalXMLDoc [read $fp]

bind . <Escape> {+exec wish $argv0 &; exit}         ;# quick restart

close $fp
set XMLDoc {<tclui name="Generic Sowing Rule">
<uiscript>source $apsuite/apsim/Economics/lib/RotationsUI.tcl</uiscript>
<canvas_height>600</canvas_height>
<rules_height>40</rules_height>
          <node>
            <x1>568.0</x1>
            <name>Chickpea</name>
            <alias>chickpea</alias>
            <y1>44.0</y1>
            <x2>668.0</x2>
            <desc>Chickpea</desc>
            <fill>lightgreen</fill>
            <y2>144.0</y2>
          </node>
          <node>
            <x1>194.0</x1>
            <name>SummerFallow2</name>
            <alias>fallow</alias>
            <y1>406.0</y1>
            <x2>294.0</x2>
            <desc>New state</desc>
            <fill>gray</fill>
            <y2>506.0</y2>
          </node>
          <node>
            <x1>427.0</x1>
            <name>Wheat</name>
            <alias>wheat</alias>
            <y1>437.0</y1>
            <x2>527.0</x2>
            <desc>Wheat Crop</desc>
            <fill>darkgreen</fill>
            <y2>537.0</y2>
          </node>
          <node>
            <x1>121.0</x1>
            <name>WinterFallow</name>
            <alias>fallow</alias>
            <y1>228.0</y1>
            <x2>221.0</x2>
            <desc>New state</desc>
            <fill>gray</fill>
            <y2>328.0</y2>
          </node>
          <node>
            <x1>173.0</x1>
            <name>Cotton</name>
            <alias>cotton</alias>
            <y1>53.0</y1>
            <x2>273.0</x2>
            <desc>A Cotton Crop</desc>
            <fill>purple</fill>
            <y2>153.0</y2>
          </node>
          <node>
            <x1>368.0</x1>
            <name>F1</name>
            <alias>fallow</alias>
            <y1>4.0</y1>
            <x2>468.0</x2>
            <desc>New state</desc>
            <fill>gray</fill>
            <y2>104.0</y2>
          </node>
          <node>
            <x1>563.0</x1>
            <name>SummerFallow1</name>
            <alias>fallow</alias>
            <y1>277.0</y1>
            <x2>663.0</x2>
            <desc>New state</desc>
            <fill>maroon</fill>
            <y2>377.0</y2>
          </node>
          <arc>
            <name>arc6</name>
            <source>WinterFallow</source>
            <target>Cotton</target>
            <x>138.5</x>
            <actions>apsimSendMessage cotton sow {plants_pm 10} {sowing_depth  30} {cultivar  siok} {row_spacing 1000}</actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <y>174.5</y>
            <rules>[dateWithin 288 349]</rules>
            <rules>[sumLastRain 4] &gt; 10</rules>
            <rules>[apsimGet esw] &gt; 100</rules>
          </arc>
          <arc>
            <name>arc2</name>
            <source>SummerFallow1</source>
            <target>WinterFallow</target>
            <x>353.5</x>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <y>499.0</y>
            <rules>[apsimGet day]  == 192</rules>
          </arc>
          <arc>
            <name>arc7</name>
            <source>F1</source>
            <target>SummerFallow1</target>
            <x>591.5</x>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <y>123.0</y>
            <rules>[apsimGet day]  == 192</rules>
          </arc>
          <arc>
            <name>arc3</name>
            <source>Wheat</source>
            <target>SummerFallow2</target>
            <x>363.0</x>
            <actions>apsimSendMessage wheat harvest</actions>
            <actions>apsimSendMessage wheat end_crop</actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <y>522.5</y>
            <rules>{[apsimGet wheat.stage_name] == "harvest_ripe" || [apsimGet wheat.plant_status] == "dead"}</rules>
          </arc>
          <arc>
            <name>arc8</name>
            <source>Chickpea</source>
            <target>SummerFallow1</target>
            <x>647.0</x>
            <actions>apsimSendMessage chickpea harvest</actions>
            <actions>apsimSendMessage chickpea end_crop</actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <y>210.5</y>
            <rules>{[apsimGet chickpea.stage_name] == "harvest_ripe" || [apsimGet chickpea.plant_status] == "dead"}</rules>
          </arc>
          <arc>
            <name>arc0</name>
            <source>F1</source>
            <target>Chickpea</target>
            <x>521.0</x>
            <actions>apsimSendMessage chickpea sow {plants 25} {sowing_depth 30} {cultivar amethyst} {row_spacing 500} </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <y>39.0</y>
            <rules>[dateWithin 135 191]</rules>
            <rules>[sumLastRain 3] &gt; 25</rules>
            <rules>[apsimGet esw] &gt; 50</rules>
          </arc>
          <arc>
            <name>arc9</name>
            <source>SummerFallow2</source>
            <target>WinterFallow</target>
            <x>164.0</x>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <y>385.5</y>
            <rules>[apsimGet day]  == 74</rules>
          </arc>
          <arc>
            <name>arc10</name>
            <source>WinterFallow</source>
            <target>Cotton</target>
            <x>235.5</x>
            <actions>apsimSendMessage cotton sow {plants_pm 10} {sowing_depth  30} {cultivar  siok} {row_spacing 1000}</actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <y>210.5</y>
            <rules>[apsimGet day] == 350</rules>
          </arc>
          <arc>
            <name>arc5</name>
            <source>Cotton</source>
            <target>F1</target>
            <x>288.5</x>
            <actions>apsimSendMessage cotton harvest</actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <y>34.5</y>
            <rules>[apsimGet ozcot_status] &gt; 0</rules>
          </arc>
          <arc>
            <name>arc1</name>
            <source>SummerFallow1</source>
            <target>Wheat</target>
            <x>576.5</x>
            <actions>apsimSendMessage wheat sow {plants 100} {sowing_depth 30} {cultivar hartog} {row_spacing 250} </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <actions>
            </actions>
            <y>410.5</y>
            <rules>[dateWithin 135 191]</rules>
            <rules>[sumLastRain 3] &gt; 25</rules>
            <rules>[apsimGet esw] &gt; 50</rules>
          </arc>
          <rule name="Init rule" invisible="yes" condition="init"><![CDATA[package require struct
::struct::graph stateGraph
stateGraph node insert "Chickpea"
set alias(Chickpea) "chickpea"
set colour(Chickpea) "lightgreen"
stateGraph node insert "SummerFallow2"
set alias(SummerFallow2) "fallow"
set colour(SummerFallow2) "gray"
stateGraph node insert "Wheat"
set alias(Wheat) "wheat"
set colour(Wheat) "darkgreen"
stateGraph node insert "WinterFallow"
set alias(WinterFallow) "fallow"
set colour(WinterFallow) "gray"
stateGraph node insert "Cotton"
set alias(Cotton) "cotton"
set colour(Cotton) "purple"
stateGraph node insert "F1"
set alias(F1) "fallow"
set colour(F1) "gray"
stateGraph node insert "SummerFallow1"
set alias(SummerFallow1) "fallow"
set colour(SummerFallow1) "maroon"
stateGraph arc insert "WinterFallow" "Cotton" "arc6"
stateGraph arc lappend "arc6" actions {apsimSendMessage cotton sow {plants_pm 10} {sowing_depth  30} {cultivar  siok} {row_spacing 1000}}
stateGraph arc lappend "arc6" actions {}
stateGraph arc lappend "arc6" actions {}
stateGraph arc lappend "arc6" actions {}
stateGraph arc lappend "arc6" actions {}
stateGraph arc lappend "arc6" actions {}
stateGraph arc lappend "arc6" actions {}
stateGraph arc lappend "arc6" rules {[dateWithin 288 349]}
stateGraph arc lappend "arc6" rules {[sumLastRain 4] > 10}
stateGraph arc lappend "arc6" rules {[apsimGet esw] > 100}
stateGraph arc insert "SummerFallow1" "WinterFallow" "arc2"
stateGraph arc lappend "arc2" actions {}
stateGraph arc lappend "arc2" actions {}
stateGraph arc lappend "arc2" actions {}
stateGraph arc lappend "arc2" rules {[apsimGet day]  == 192}
stateGraph arc insert "F1" "SummerFallow1" "arc7"
stateGraph arc lappend "arc7" actions {}
stateGraph arc lappend "arc7" actions {}
stateGraph arc lappend "arc7" actions {}
stateGraph arc lappend "arc7" rules {[apsimGet day]  == 192}
stateGraph arc insert "Wheat" "SummerFallow2" "arc3"
stateGraph arc lappend "arc3" actions {apsimSendMessage wheat harvest}
stateGraph arc lappend "arc3" actions {apsimSendMessage wheat end_crop}
stateGraph arc lappend "arc3" actions {}
stateGraph arc lappend "arc3" actions {}
stateGraph arc lappend "arc3" actions {}
stateGraph arc lappend "arc3" actions {}
stateGraph arc lappend "arc3" actions {}
stateGraph arc lappend "arc3" actions {}
stateGraph arc lappend "arc3" actions {}
stateGraph arc lappend "arc3" actions {}
stateGraph arc lappend "arc3" rules {{[apsimGet wheat.stage_name] == "harvest_ripe" || [apsimGet wheat.plant_status] == "dead"}}
stateGraph arc insert "Chickpea" "SummerFallow1" "arc8"
stateGraph arc lappend "arc8" actions {apsimSendMessage chickpea harvest}
stateGraph arc lappend "arc8" actions {apsimSendMessage chickpea end_crop}
stateGraph arc lappend "arc8" actions {}
stateGraph arc lappend "arc8" actions {}
stateGraph arc lappend "arc8" actions {}
stateGraph arc lappend "arc8" actions {}
stateGraph arc lappend "arc8" rules {{[apsimGet chickpea.stage_name] == "harvest_ripe" || [apsimGet chickpea.plant_status] == "dead"}}
stateGraph arc insert "F1" "Chickpea" "arc0"
stateGraph arc lappend "arc0" actions {apsimSendMessage chickpea sow {plants 25} {sowing_depth 30} {cultivar amethyst} {row_spacing 500} }
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" actions {}
stateGraph arc lappend "arc0" rules {[dateWithin 135 191]}
stateGraph arc lappend "arc0" rules {[sumLastRain 3] > 25}
stateGraph arc lappend "arc0" rules {[apsimGet esw] > 50}
stateGraph arc insert "SummerFallow2" "WinterFallow" "arc9"
stateGraph arc lappend "arc9" actions {}
stateGraph arc lappend "arc9" actions {}
stateGraph arc lappend "arc9" actions {}
stateGraph arc lappend "arc9" actions {}
stateGraph arc lappend "arc9" actions {}
stateGraph arc lappend "arc9" rules {[apsimGet day]  == 74}
stateGraph arc insert "WinterFallow" "Cotton" "arc10"
stateGraph arc lappend "arc10" actions {apsimSendMessage cotton sow {plants_pm 10} {sowing_depth  30} {cultivar  siok} {row_spacing 1000}}
stateGraph arc lappend "arc10" actions {}
stateGraph arc lappend "arc10" actions {}
stateGraph arc lappend "arc10" actions {}
stateGraph arc lappend "arc10" actions {}
stateGraph arc lappend "arc10" actions {}
stateGraph arc lappend "arc10" actions {}
stateGraph arc lappend "arc10" rules {[apsimGet day] == 350}
stateGraph arc insert "Cotton" "F1" "arc5"
stateGraph arc lappend "arc5" actions {apsimSendMessage cotton harvest}
stateGraph arc lappend "arc5" actions {}
stateGraph arc lappend "arc5" actions {}
stateGraph arc lappend "arc5" actions {}
stateGraph arc lappend "arc5" actions {}
stateGraph arc lappend "arc5" actions {}
stateGraph arc lappend "arc5" actions {}
stateGraph arc lappend "arc5" actions {}
stateGraph arc lappend "arc5" actions {}
stateGraph arc lappend "arc5" rules {[apsimGet ozcot_status] > 0}
stateGraph arc insert "SummerFallow1" "Wheat" "arc1"
stateGraph arc lappend "arc1" actions {apsimSendMessage wheat sow {plants 100} {sowing_depth 30} {cultivar hartog} {row_spacing 250} }
stateGraph arc lappend "arc1" actions {}
stateGraph arc lappend "arc1" actions {}
stateGraph arc lappend "arc1" actions {}
stateGraph arc lappend "arc1" actions {}
stateGraph arc lappend "arc1" actions {}
stateGraph arc lappend "arc1" actions {}
stateGraph arc lappend "arc1" rules {[dateWithin 135 191]}
stateGraph arc lappend "arc1" rules {[sumLastRain 3] > 25}
stateGraph arc lappend "arc1" rules {[apsimGet esw] > 50}
]]></rule>
</tclui>}

source  "C:/development/apsim/Economics/lib/RotationsUI.tcl"

