set apsuite c:/development
set fp [open "C:/Documents and Settings/devoilp/My Documents/zzz.apsim" r]
set GlobalXMLDoc [read $fp]

bind . <Escape> {+exec wish $argv0 &; exit}         ;# quick restart

close $fp
set XMLDoc {<tclui name="Generic Sowing Rule">
      <uiscript>source $apsuite/apsim/Economics/lib/GenericSowingUI.tcl</uiscript>
      <crop>wheat</crop>
      <ruletemplate name="Sowing rule - start_of_day" condition="start_of_day"><![CDATA[
if (paddock_is_fallow() = 1 and FallowIn <> 'yes' and (NextCrop = 0 or NextCrop = '[crop]')) then
  if (date_within('[date1], [date2]') = 1) then
    if (rain[[rainnumdays]] >= [raincrit] AND 
        esw >= [esw_amount] ) <> 0) OR
        ('[must_sow]' = 'yes' AND today = date('[date2]'))) THEN
      ChooseNextCrop = 'yes'   ! for rotations

      [crop] sow plants =[density], sowing_depth = [depth], cultivar = [cultivar], row_spacing = [row_spacing], crop_class = [class]

    endif
    if today = date('[date2]') then
      ChooseNextCrop = 'yes'
    endif
  endif
endif
]]></ruletemplate>
      <date1>1-jun</date1>
      <date2>1-aug</date2>
      <criteria>variable</criteria>
      <cultivar>hartog</cultivar>
      <additionalParameter>zzzzzzzzzz</additionalParameter>
      </tclui>}

source  "C:/development/apsim/economics/lib/GenericSowingUI.tcl"

