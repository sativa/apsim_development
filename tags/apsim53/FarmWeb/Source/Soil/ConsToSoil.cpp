//---------------------------------------------------------------------------


#pragma hdrstop

#include "ConsToSoil.h"
#include <vcl.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <general\IniFile.h>
#include <general\string_functions.h>
#include "TMissingParametersForm.h"
//---------------------------------------------------------------------------

#pragma package(smart_init)


//------------------------------------------------------------------------------
// Get parameter values for specified parameter
//------------------------------------------------------------------------------
bool ConsToSoil::getParameterValue(IniFile& par,
                                   const string& sectionName,
                                   const string& parameterName,
                                   string& value)
   {
   par.read(sectionName, parameterName, value);
   if (value != "")
      {
      splitOffBracketedValue(value, '(', ')');
      return true;
      }
   else
      {
      missingParameters.push_back("Section: "+sectionName+ " Parameter: " + parameterName);
      return false;
      }
   }
//------------------------------------------------------------------------------
// Get parameter values for specified parameter
//------------------------------------------------------------------------------
bool ConsToSoil::getParameterValues(IniFile& par,
                                    const string& sectionName,
                                    const string& parameterName,
                                    vector<string>& values)
   {
   string value;
   if (getParameterValue(par, sectionName, parameterName, value))
      {
      splitIntoValues(value, " ", values);
      return true;
      }
   else
      return false;
   }

//------------------------------------------------------------------------------
//  Display all issues that arrized during conversion
//------------------------------------------------------------------------------
void ConsToSoil::displayMessages(string messages)
   {
   if(missingParameters.size() > 0 )
      {
      sort(missingParameters.begin(), missingParameters.end());
      TMissingParametersForm* form = new TMissingParametersForm(NULL);

      form->Memo->Lines->Clear();
      for(unsigned i = 0; i < missingParameters.size(); i++)
         form->Memo->Lines->Add(missingParameters[i].c_str());
      form->ShowModal();
      delete form;
      }
   }

//------------------------------------------------------------------------------
// Imports an APSIM parameter file.
//------------------------------------------------------------------------------
void ConsToSoil::importFromFile(string& fileName, Soils& soils)
   {
   try
      {
      IniFile par(fileName);
      vector<string> sectionNames;
      par.readSectionNames(sectionNames);

      // Import all water sections first.
      for (unsigned s = 0; s != sectionNames.size(); s++)
         {
         if (stristr(sectionNames[s].c_str(), ".soilwat2.") != NULL)
            importSection(par, sectionNames[s], soils);
         }
      // Next import all nitrogen sections.
      for (unsigned s = 0; s != sectionNames.size(); s++)
         {
         if (stristr(sectionNames[s].c_str(), ".soiln2.") != NULL)
            importSection(par, sectionNames[s], soils);
         }

      // Next import all other sections - may be a crop.
      for (unsigned s = 0; s != sectionNames.size(); s++)
         {
         if (stristr(sectionNames[s].c_str(), ".soilwat2.") == NULL &&
             stristr(sectionNames[s].c_str(), ".soiln2.") == NULL)
            importSection(par, sectionNames[s], soils);
         }
      displayMessages("Messages: \n");
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      }
   }
//------------------------------------------------------------------------------
// Imports an APSIM parameter file.
//------------------------------------------------------------------------------
void ConsToSoil::importSection(IniFile& par, string& sectionName, Soils& soils)
   {
   vector<string> sectionNameBits;
   splitIntoValues(sectionName, ".", sectionNameBits);
   if (sectionNameBits.size() == 3)
      {
      string newSoilName = sectionNameBits[0];
      Soil* newSoilPtr = NULL;
      bool alreadyExists;
      try
         {
         newSoilPtr = soils.get(newSoilName);
         alreadyExists = true;
         }
      catch (const exception& err)
         {
         newSoilPtr = new Soil;
         newSoilPtr->setName(newSoilName);
         alreadyExists = false;
         }
      Soil& newSoil = *newSoilPtr;

      string moduleName = sectionNameBits[1];

      // Water section
      if(Str_i_Eq(moduleName, "soilwat2"))
         {
         // get water variables.
         vector <double> thicknessAsDoubles;
         if (getParameterValues(par, sectionName, "dlayer", thicknessAsDoubles))
            {
            vector<unsigned> thickness;
            for (unsigned l = 0; l != thicknessAsDoubles.size(); l++)
               thickness.push_back(thicknessAsDoubles[l]);
            newSoil.setThickness(thickness);
            }
         double value;
         if (getParameterValue(par, sectionName, "cona", value))
            newSoil.setCona(value);
         if (getParameterValue(par, sectionName, "diffus_const", value))
            newSoil.setDiffusConst(value);
         if (getParameterValue(par, sectionName, "diffus_slope", value))
            newSoil.setDiffusSlope(value);
         if (getParameterValue(par, sectionName, "u", value))
            newSoil.setU(value);
         if (getParameterValue(par, sectionName, "salb", value))
            newSoil.setSalb(value);
         if (getParameterValue(par, sectionName, "cn2_bare", value))
            newSoil.setCn2Bare(value);
         if (getParameterValue(par, sectionName, "cn_red", value))
            newSoil.setCnRed(value);
         if (getParameterValue(par, sectionName, "cn_cov", value))
            newSoil.setCnCov(value);

         vector<double> values;
         if (getParameterValues(par, sectionName, "air_dry", values))
            newSoil.setAirdry(values);
         if (getParameterValues(par, sectionName, "ll15", values))
            newSoil.setLl15(values);
         if (getParameterValues(par, sectionName, "dul", values))
            newSoil.setDul(values);
         if (getParameterValues(par, sectionName, "sat", values))
            newSoil.setSat(values);
         if (getParameterValues(par, sectionName, "sw", values))
            newSoil.setSw(values);
         if (getParameterValues(par, sectionName, "swcon", values))
            newSoil.setSwcon(values);
         if (getParameterValues(par, sectionName, "bd", values))
            newSoil.setBd(values);
         }


      // Nitrogen section
      else if(Str_i_Eq(moduleName, "soiln2"))
         {
         if (newSoil.thickness().size() > 0)
            {
            double value;
            if (getParameterValue(par, sectionName, "root_cn", value))
               newSoil.setRootCN(value);
            if (getParameterValue(par, sectionName, "root_wt", value))
               newSoil.setRootWt(value);
            if (getParameterValue(par, sectionName, "soil_cn", value))
               newSoil.setSoilCN(value);
            if (getParameterValue(par, sectionName, "enr_a_coeff", value))
               newSoil.setEnrAcoeff(value);
            if (getParameterValue(par, sectionName, "enr_b_coeff", value))
               newSoil.setEnrBcoeff(value);

            vector<double> values;
            if (getParameterValues(par, sectionName, "oc", values))
               newSoil.setOc(values);
            if (getParameterValues(par, sectionName, "ph", values))
               newSoil.setPh(values);
            if (getParameterValues(par, sectionName, "fbiom", values))
               newSoil.setFbiom(values);
            if (getParameterValues(par, sectionName, "finert", values))
               newSoil.setFinert(values);
            if (getParameterValues(par, sectionName, "no3ppm", values))
               newSoil.setNo3(values);
            if (getParameterValues(par, sectionName, "nh4ppm", values))
               newSoil.setNh4(values);
            }
         }


      // Potential crop section.
      else
         {
         if (newSoil.thickness().size() > 0)
            {
            string ll;
            par.read(sectionName, "ll", ll);
            if (ll != "")
               {
               newSoil.addCrop(moduleName);
               vector<double> values;
               if (getParameterValues(par, sectionName, "ll", values))
                  newSoil.setll(moduleName, values);
               if (getParameterValues(par, sectionName, "kl", values))
                  newSoil.setkl(moduleName, values);
               if (getParameterValues(par, sectionName, "xf", values))
                  newSoil.setxf(moduleName, values);
               }
            }
         }

      if (newSoil.thickness().size() > 0)
         {
         if (alreadyExists)
            soils.erase(newSoilName);
         soils.add(newSoil);
         }
      delete newSoilPtr;
      }
   }

