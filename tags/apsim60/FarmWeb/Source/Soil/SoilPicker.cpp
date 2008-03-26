//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "SoilPicker.h"
#include "TCharacterisationForm.h"
#include "Soils.h"
#include "TSoilPickerForm.h"
#include <ApsimShared\ApsimSettings.h>
#include <general\io_functions.h>

#pragma package(smart_init)


static const char* W2_TEMPLATE =
   "!Title = .name\n"
   "[*attributes]\n"
   "   module_usage  = soil_water\n"
   "   must_have     = soil_water\n"
   "\n"
   "[*contents]\n"
   "#for_each soil.crop\n"
   "#if (\"crop.name\" = \"cotton\")\n"
   "[run%.ozcot.parameters]\n"
   "   ! Predicted values: crop.predicted\n"
   "   ll  = crop.ll       ! Crop lower limits for each layer\n"
   "   title = XXXX\n"
   "   asoil = 3.0\n"
   "#else\n"
   "[run%.crop.name.parameters]\n"
   "   ! Predicted values: crop.predicted\n"
   "   ll  = crop.ll       ! Crop lower limits for each layer\n"
   "   kl  = crop.kl       ! Water Extraction parameter (0-1)\n"
   "   xf  = crop.xf       ! Root Exploration factor (0-1)\n"
   "#endif\n"
   "#endfor\n"
   "[default.soilwat2.parameters]\n"
   "   #for_each soil.water\n"
   "   diffus_const = water.diffusconst   ! coeffs for unsaturated water flow\n"
   "   diffus_slope = water.diffusslope\n"
   "   cn2_bare     = $cn2    ! bare soil runoff curve number\n"
   "   cn_red       = water.cnred    ! potetial reduction in curve number due to residue\n"
   "   cn_cov       = water.cncov   ! cover for maximum reduction in curve number\n"
   "   salb         = water.salb  ! bare soil albedo\n"
   "   cona         = $cona     ! stage 2 evap coef.\n"
   "   u            = $uritch     ! stage 1 soil evaporation coefficient (mm)\n"
   "   #endfor\n"
   "\n"
   "   dlayer  = .thickness   ! layer thickness mm soil\n"
   "   air_dry = .airdry   ! air dry mm water/ mm soil\n"
   "   ll15    = .ll15   ! lower limit mm water/mm soil\n"
   "   dul     = .dul   ! drained upper limit mm water/mm soil\n"
   "   sat     = .sat   ! saturation mm water/mm soil\n"
   "   swcon   = .swcon   ! drainage coefficient\n"
   "   bd      = .bd   ! bulk density gm dry soil/cc moist soil\n"
   "\n"
   "   #for_each soil.water\n"
   "[*variables]\n"
   "   $cona = \"Cona : \" water.cona real\n"
   "   $uritch = \"Uritch : \" water.u real\n"
   "   $cn2 = \"Runoff curve number for BARE soil : \" water.cn2bare real\n"
   "   #endfor\n";

static const char* N2_TEMPLATE =
   "!Title = .name\n"
   "[*attributes]\n"
   "   module_usage  = soil_nitrogen\n"
   "   must_have     = soil_nitrogen\n"
   "\n"
   "[*contents]\n"
   "[default.soiln2.parameters]\n"
   "   #for_each soil.nitrogen\n"
   "   root_cn      = nitrogen.rootcn     ! C:N ratio of initial root residues\n"
   "   root_wt      = nitrogen.rootwt   ! root residues as biomass (kg/ha)\n"
   "   soil_cn      = nitrogen.soilcn   ! C:N ratio of soil\n"
   "   enr_a_coeff  = nitrogen.enracoeff\n"
   "   enr_b_coeff  = nitrogen.enrbcoeff\n"
   "   profile_reduction =  off\n"
   "   #endfor\n"
   "\n"
   "#if (\".oc\" = \".oc\")\n"
   "   oc      = 1.24    1.24    1.25    1.10    0.71    0.34    0.26  (%)   ! organic carbon %\n"
   "#else\n"
   "   oc      = .oc   ! Soil Organic Carbon\n"
   "#endif\n"
   "#if (\".ph\" = \".ph\")\n"
   "   ph      = 7.00    7.00    7.00    7.00    7.00    7.00    7.00  ()    ! ph\n"
   "#else\n"
   "   ph      = .ph   ! pH of soil\n"
   "#endif\n"
   "   fbiom   = .fbiom   ! Organic C Biomass Fraction\n"
   "   finert  = .finert   ! Inert Organic C Fraction\n"
   "   no3ppm  = .no3ppm   ! Nitrate Concentration\n"
   "   nh4ppm  = .nh4ppm   ! Ammonium Concentration\n"
   "\n";


// ------------------------------------------------------------------
// Return a refno from a soil template title. Refno's appear after a
// '#' character. Return true if found.
// ------------------------------------------------------------------
int getRefNoFromTitle(const string& title)
   {
   unsigned posHash = title.find("#");
   if (posHash != string::npos)
      return atoi(title.substr(posHash+1).c_str());
   else
      return 0;
   }

//---------------------------------------------------------------------------
// Go find a soil in the specified soils database and return it's title.
//---------------------------------------------------------------------------
string findSoil(Soils& soils, const string& title)
   {
   // try and locate the title as is.
   try
      {
      soils.get(title);
      return title;
      }
   catch (const exception& err)
      {
      }

   int refno = getRefNoFromTitle(title);

   ostringstream refNoString;
   refNoString << '#';
   refNoString.fill('0');
   refNoString.width(3);
   refNoString << refno;
   vector<string> soilNames = soils.names();
   for (unsigned s = 0; s != soilNames.size(); s++)
      {
      if (soilNames[s].find(refNoString.str()) == 0)
         return soilNames[s];
      }
   return "";
   }
//---------------------------------------------------------------------------
// Shows a modal dialog box and allows the user to pick a soil. Returns true
// if a soil was picked.
//---------------------------------------------------------------------------
extern "C" _stdcall _export DWORD PickSoil(char* dbFileName, const char* title, char* w2FileName, char* n2FileName)
   {
   strcpy(w2FileName, GetTempDir().c_str());
   strcat(w2FileName, "\\temp.w2");
   strcpy(n2FileName, GetTempDir().c_str());
   strcat(n2FileName, "\\temp.n2");

   // Get name of soil database and tell an instance of CharactData to open it.
   string soilFile;
   if (strlen(dbFileName) == 0)
      {
      ApsimSettings settings;
      settings.read("Soil|Soil file", soilFile, true);
      }
   else
      soilFile = dbFileName;

   Soils soils;
   soils.open(soilFile);

   string defaultSoilName = findSoil(soils, title);
   if (defaultSoilName == "")
      {
      string msg = string("Cannot find soil '") + title + "' in soil database " + soilFile;
      MessageBox(NULL, msg.c_str(), "Warning", MB_ICONINFORMATION | MB_OK);
      }

   TSoilPickerForm* form = new TSoilPickerForm((TComponent*)NULL);
   form->setup(soilFile, defaultSoilName, true, true);
   if (form->ShowModal() == mrOk)
      {
      try
         {
         soils.open(form->getCurrentDbFileName());
         Soil* soil = soils.get(form->getCurrentSoilNameSelected());
         ofstream w2(w2FileName);
         soil->exportSoil(w2, W2_TEMPLATE, false);
         ofstream n2(n2FileName);
         soil->exportSoil(n2, N2_TEMPLATE, false);
         strcpy(dbFileName, form->getCurrentDbFileName().c_str());
         delete soil;
         delete form;
         return true;
         }
      catch (const std::runtime_error& err)
         {
         MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
         }
      }
   delete form;
   return false;
   }
//---------------------------------------------------------------------------
// Shows a modal dialog box and allows the user to pick a soil. The APSIM
// parameter files for that soil are then returned.  Returns true if a soil
// was picked.
//---------------------------------------------------------------------------
extern "C" bool _stdcall _export getSoil(const char* dbFileName, const char* title, char* w2FileName, char* n2FileName)
   {
   strcpy(w2FileName, GetTempDir().c_str());
   strcat(w2FileName, "\\temp.w2");
   strcpy(n2FileName, GetTempDir().c_str());
   strcat(n2FileName, "\\temp.n2");

   // Get name of soil database and tell an instance of CharactData to open it.
   string soilFile;
   if (strlen(dbFileName) == 0)
      {
      ApsimSettings settings;
      settings.read("Soil|Soil file", soilFile, true);
      }
   else
      soilFile = dbFileName;

   Soils soils;
   soils.open(soilFile);

   string defaultSoilName = findSoil(soils, title);
   if (defaultSoilName == "")
      return false;

   try
      {
      Soil* soil = soils.get(defaultSoilName);
      ofstream w2(w2FileName);
      soil->exportSoil(w2, W2_TEMPLATE, false);
      ofstream n2(w2FileName);
      soil->exportSoil(n2, N2_TEMPLATE, false);
      delete soil;
      return true;
      }
   catch (const std::runtime_error& err)
      {
      return false;
      }
   }

