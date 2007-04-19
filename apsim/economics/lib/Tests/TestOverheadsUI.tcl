set apsuite c:/development
set fp [open "C:/Documents and Settings/devoilp/My Documents/zzz.apsim" r]
set GlobalXMLDoc [read $fp]

bind . <Escape> {+exec wish $argv0 &; exit}         ;# quick restart

close $fp
set XMLDoc {<tclui name="Generic Sowing Rule">
<uiscript>source $apsuite/apsim/economics/lib/OverheadsUI.tcl</uiscript>
      <category name="Overhead Expenses">overhead</category>
      <overhead>
        <description>Accountant/Consultancy fees</description>
        <name>accountancy</name>
        <value>5000</value>
      </overhead>
      <overhead>
        <description>Administration</description>
        <name>administration</name>
        <value>500</value>
      </overhead>
      <overhead>
        <description>Bank Charges other than interest</description>
        <name>bankcharges</name>
        <value>500</value>
      </overhead>
      <overhead>
        <description>Electricity - Farm</description>
        <name>electricity</name>
        <value>2000</value>
      </overhead>
      <overhead>
        <description>Freight &amp; Cartage (sundry)</description>
        <name>freight</name>
        <value>0</value>
      </overhead>
      <overhead>
        <description>Fuel &amp; Oil (other than farming)</description>
        <name>fuels</name>
        <value>10000</value>
      </overhead>
      <overhead>
        <description>Insurance - Farm</description>
        <name>insurance</name>
        <value>6000</value>
      </overhead>
      <overhead>
        <description>Motor Vehicle Expenses - Farm</description>
        <name>vehicleexpenses</name>
        <value>5000</value>
      </overhead>
      <overhead>
        <description>Rates and Rents - Farm</description>
        <name>rates</name>
        <value>3500</value>
      </overhead>
      <overhead>
        <description>Repairs &amp; Maintenance (extra)</description>
        <name>repairs</name>
        <value>20000</value>
      </overhead>
      <overhead>
        <description>Subscriptions</description>
        <name>subs</name>
        <value>1000</value>
      </overhead>
      <overhead>
        <description>Telephone - Farm</description>
        <name>telephone</name>
        <value>2500</value>
      </overhead>
      <overhead>
        <description>Wages (other than casual included in activity budgets)</description>
        <name>wages</name>
        <value>30000</value>
      </overhead>
      <overhead>
        <description>Other Overhead Expenses</description>
        <name>other</name>
        <value>0</value>
      </overhead>
      <overhead>
        <description>Operators labour and management</description>
        <name>operator</name>
        <value>50000</value>
      </overhead>
</tclui>}

source  "C:/development/apsim/economics/lib/OverheadsUI.tcl"


button .test -text "Test" -command "setXML junk junk junk" 
grid .test 



