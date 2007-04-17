set apsuite c:/development
set fp [open "C:/Documents and Settings/devoilp/My Documents/zzz.apsim" r]
set GlobalXMLDoc [read $fp]

bind . <Escape> {+exec wish $argv0 &; exit}         ;# quick restart

close $fp
set XMLDoc {<tclui name="Costs - Fertilisers">
      <uiscript>source $apsuite/apsim/Economics/lib/TreatmentsUI.tcl</uiscript>
      <category>fertilisercost</category>
      <fertilisercost>
        <desc>Big n (82n)</desc>
        <name>big_n</name>
        <price>0.61</price>
        <units>$/kg</units>
        <updated />
      </fertilisercost>
      <fertilisercost>
        <desc>CK700 (32.3n;5.1p;18.9s)</desc>
        <name>ck700</name>
        <price>0.52</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>CK1 (14.6p;14.5k;1s;10.6ca)</desc>
        <name>ck1</name>
        <price>0.52</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>CK600s (17.9n;5.1p;18.9s)</desc>
        <name>ck600s</name>
        <price>0.52</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>DAP (18n;20p)</desc>
        <name>dap</name>
        <price>0.48</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>granam (20.2n;24s)</desc>
        <name>granam</name>
        <price>0.48</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>granulock (starter z - 10.5n;19.5p;2.2s)</desc>
        <name>granulock</name>
        <price>0.62</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>Gypsum (14.5s;18.5ca)</desc>
        <name>gypsum</name>
        <price>0.07</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>Lime (Ca)</desc>
        <name>lime</name>
        <price>0.05</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>MAP (starterphos - 10n;21.9p;1.5s)</desc>
        <name>map</name>
        <price>0.52</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>Muriate of Potash (mop - 50k)</desc>
        <name>mop</name>
        <price>0.51</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>nitram (34N)</desc>
        <name>nitram</name>
        <price>0.71</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>Nitram and Sulphur</desc>
        <name>nitram_and_sulphur</name>
        <price>0.13</price>
        <units>$/kg</units>
        <updated />
      </fertilisercost>
      <fertilisercost>
        <desc>Phosul</desc>
        <name>phosul</name>
        <price>0.57</price>
        <units>$/kg</units>
        <updated />
      </fertilisercost>
      <fertilisercost>
        <desc>potassium nitrate (13n;38.3k)</desc>
        <name>potassium_nitrate</name>
        <price>0.95</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>solubor (20.5b)</desc>
        <name>solubor</name>
        <price>3.83</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>starter z</desc>
        <name>starter_z</name>
        <price>0.62</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>starterphos (10n;21.9p;1.5s)</desc>
        <name>starterphos</name>
        <price>0.48</price>
        <units>$/kg</units>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>sulphate of potash (41K;18S)</desc>
        <name>sulphate_of_potash</name>
        <price>0.69</price>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>Supazinc (7.5Zn)</desc>
        <name>supazinc</name>
        <price>4.82</price>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>Superphosphate (8.8P;11S;20Ca)</desc>
        <name>superphosphate</name>
        <price>0.34</price>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>tech feed </desc>
        <name>tech_feed</name>
        <price>0.58</price>
        <updated>needs to be specific</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>Urea (46N)</desc>
        <name>urea</name>
        <price>0.5</price>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>zinc sulphate heptahydrate</desc>
        <name>zinc_sulphate_heptahydrate</name>
        <price>0.72</price>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>zinc sulphate monohydrate (35Zn;17.2S)</desc>
        <name>zinc sulphate monohydrate</name>
        <price>1.12</price>
        <updated>30/03/2004</updated>
      </fertilisercost>
      <fertilisercost>
        <desc>Ammonium Sulphate</desc>
        <name>ammonium_sulphate</name>
        <price>0.5</price>
        <updated />
      </fertilisercost>
    </tclui>}

source  "C:/development/apsim/economics/lib/TreatmentsUI.tcl"


button .test -text "Test" -command "setXML junk junk junk" 
grid .test 



