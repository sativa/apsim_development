//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "SampleData.h"
#include "CharactData.h"
#include <general\path.h>
#include <general\io_functions.h>
#include <general\math_functions.h>
#include <numeric>
#include <sstream>
#pragma package(smart_init)

typedef enum ADCPROP_UPDATERESYNC_ENUM
{
  adResyncNone = 0,
  adResyncAutoIncrement = 1,
  adResyncConflicts = 2,
  adResyncUpdates = 4,
  adResyncInserts = 8,
  adResyncAll = 15
} ADCPROP_UPDATERESYNC_ENUM;

// ------------------------------------------------------------------
// Return a calculated depth thicknesses (mm)
//     eg.  Depths
//            15
//            30
//            60
//            90
//           120
//     gives:
//          Thickness
//             15
//             15
//             30
//             30
//             30
// ------------------------------------------------------------------
void calcThickness (const vector<unsigned>& depths,
                    vector<unsigned>& thickness)
   {
   for (unsigned layer = 0; layer != depths.size(); layer++)
      {
      if (layer == 0)
         thickness.push_back(depths[layer]);
      else
         thickness.push_back( (depths[layer] - depths[layer-1]) );
      }
   }
// ------------------------------------------------------------------
//  Return a calculated layer mid point (mm) ie depth center
//  eg.  Depths
//         15
//         30
//         60
//         90
//        120
//  gives
//       depthCenter
//         7.5
//        22.5
//          45
//          75
//         105
// ------------------------------------------------------------------
void calcDepthCenter(const vector<unsigned>& depths,
                     vector<double>& depthCenter)
   {
   for (unsigned layer = 0; layer != depths.size(); layer++)
      {
      if (layer == 0)
         depthCenter.push_back(depths[layer] / 2.0);
      else
         depthCenter.push_back( (depths[layer] + depths[layer-1]) / 2.0);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     Return an interpolated set of values in TargetNumbers.  SourceDepths
//     and SourceNumbers are usually the characterisation data.  TargetDepths
//     are usually the sample depths that we want to interpolate to.  Retrns
//     true if data was interpolated.

//  Notes:

//  Changes:
//    DPH 13/8/97

// ------------------------------------------------------------------
bool calcInterpolatedData (vector<double> SourceDepths,
                           vector<double> SourceNumbers,
                           vector<double>& TargetDepths,
                           vector<double>& TargetNumbers)
   {
   bool Interpolated = false;

   // loop through all target depths
   // loop through each sample depth.
   double StartTargetDepth = 0;
   for (unsigned int targetdepth = 0; targetdepth < TargetDepths.size(); targetdepth++)
      {
      double EndTargetDepth = TargetDepths[targetdepth];
      double TargetThickness = EndTargetDepth - StartTargetDepth;

      // for each target depth loop through all source depths and
      // calculate a number
      double CalcValue = 0;
      double StartSourceDepth = 0;
      for (unsigned int sourcedepth = 0; sourcedepth < SourceDepths.size(); sourcedepth++)
         {
         double EndSourceDepth = SourceDepths[sourcedepth];
         double SourceThickness = EndSourceDepth - StartSourceDepth;

         if (StartTargetDepth <= EndSourceDepth && TargetThickness > 0.0)
            {
            double PropOfLayer = min(EndSourceDepth - StartTargetDepth, TargetThickness);
            PropOfLayer = min(PropOfLayer, SourceThickness);
            if (sourcedepth < SourceNumbers.size())
               CalcValue += SourceNumbers[sourcedepth] * PropOfLayer;
            TargetThickness -= PropOfLayer;
            }
         StartSourceDepth = EndSourceDepth;
         }
      if (TargetThickness == 0.0)
         TargetNumbers.push_back (CalcValue / (EndTargetDepth - StartTargetDepth));
      else
         TargetNumbers.push_back (0.0);

      StartTargetDepth = EndTargetDepth;
      }
   return Interpolated;
   }
// ------------------------------------------------------------------
//  Short description:
//    convert a dataset to a vector of numbers.

//  Notes:

//  Changes:
//    DPH 20/1/2000

// ------------------------------------------------------------------
void DataSetToVector(TDataSet* DataSet, AnsiString FieldName, vector<double>& Numbers)
   {
   Numbers.erase(Numbers.begin(), Numbers.end());
   while (!DataSet->Eof)
      {
      Numbers.push_back (DataSet->FieldValues[FieldName]);
      DataSet->Next();
      }
   }

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
SampleData::SampleData(const string& filename, CharactData* charactdata)
   : fileName(filename), charactData(charactdata)
   {
   db = new TADOConnection(NULL);
   farmerTable = new TADOTable(NULL);
   farmTable = new TADOTable(NULL);
   paddockTable = new TADOTable(NULL);
   sampleTable = new TADOTable(NULL);
   profileTable = new TADOTable(NULL);
   profileDataTable = new TADOTable(NULL);
   sampleDataTable = new TADOTable(NULL);

   // Open all tables.
   string connectionString = "Provider=Microsoft.Jet.OLEDB.4.0;"
                             "Data Source=?;"
                             "Persist Security Info=False";
   Replace_all(connectionString, "?", fileName.c_str());
   db->ConnectionString = connectionString.c_str();
   db->Connected = true;
   farmerTable->Active = true;
   farmTable->Active = true;
   paddockTable->Active = true;
   sampleTable->Active = true;
   profileTable->Active = true;
   profileDataTable->Active = true;
   sampleDataTable->Active = true;
   profileTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   profileDataTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   farmerTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   farmTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   paddockTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   sampleTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   sampleDataTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
SampleData::~SampleData()
   {
   db->Connected = false;
   delete db;
   delete farmerTable;
   delete farmTable;
   delete paddockTable;
   delete sampleTable;
   delete profileTable;
   delete profileDataTable;
   delete sampleDataTable;
   }
// ------------------------------------------------------------------
//  Short description:
//    read all information based on current dataset pointers.

//  Notes:

//  Changes:
//    DPH 20/1/2000

// ------------------------------------------------------------------
void SampleData::readInfo(void)
   {
   depth.erase(depth.begin(), depth.end());
   wet.erase(wet.begin(), wet.end());
   dry.erase(dry.begin(), dry.end());
   nitrate.erase(nitrate.begin(), nitrate.end());

   TADOQuery* Query = new TADOQuery(NULL);
   Query->Connection = db;
   Query->SQL->Text = "SELECT Depth, Rep, Wet, Dry, NO3 "
                      "FROM   profileDataTable, sampleDataTable "
                      "WHERE  profileDataTable.ProfileID = " + sampleTable->FieldValues["ProfileID"] +
                      "  AND  sampleDataTable.SampleID = " + (long) sampleTable->FieldValues["ID"] +
                      "  AND  profileDataTable.DepthNum = sampleDataTable.DepthNum "
                      "ORDER BY Depth";
   Query->Open();
   while (!Query->Eof)
      {
      unsigned int rep = Query->Fields->Fields[1]->AsInteger;
      while (rep > wet.size())
         {
         wet.push_back(vector<double>());
         dry.push_back(vector<double>());
         nitrate.push_back(vector<double>());
         }
      if (rep == 1)
         depth.push_back (Query->Fields->Fields[0]->AsFloat);
      wet[rep-1].push_back(Query->Fields->Fields[2]->AsFloat);
      dry[rep-1].push_back(Query->Fields->Fields[3]->AsFloat);
      nitrate[rep-1].push_back(Query->Fields->Fields[4]->AsFloat);
      Query->Next();
      }
   delete Query;
   }

// ------------------------------------------------------------------
//  Short description:
//    Return volumetric water by layer for specified rep (mm).
//    Returns true if data was interpolated.
//    The returned numbers will have been interpolated to match the
//    target depths passed in.

//  Notes:

//  Changes:
//    DPH 20/1/2000

// ------------------------------------------------------------------
bool SampleData::calcVolumetric(int rep, vector<double>& volumetric, vector<double>& targetDepth)
   {
   return false;
/*   vector<double> targetBD;
   vector<double> targetWet;
   vector<double> targetDry;

   // calculate target BD, Wet and Dry
   bool Interpolated;
   Interpolated = calcInterpolatedData (charactData->getDepth(),
                                        charactData->getBulkDensity(),
                                        targetDepth,
                                        targetBD);
   Interpolated = calcInterpolatedData (getDepth(),
                                        wet[rep-1],
                                        targetDepth,
                                        targetWet);
   Interpolated = calcInterpolatedData (getDepth(),
                                        dry[rep-1],
                                        targetDepth,
                                        targetDry);

   if (targetDepth.size() > 0 &&
       targetBD.size() > 0 &&
       targetWet.size() > 0 &&
       targetDry.size() > 0)
      {

      // calculate gravimetric water content by weight (%)
      vector<double> gravimetric;
      gravimetric = subtract (targetWet, targetDry);
      gravimetric = devide (gravimetric, targetDry);
      multiply_value (gravimetric, 100.0);

      // calculate volumetric water content (%)
      volumetric = multiply(gravimetric, targetBD);
      return Interpolated;
      }
   return false;
*/   }
// ------------------------------------------------------------------
//  Short description:
//    Return plant available water by layer for specified rep (mm).
//    Returns true if data was interpolated.
//    The returned numbers will have been interpolated to match the
//    target depths passed in.

//  Notes:

//  Changes:
//    DPH 20/1/2000

// ------------------------------------------------------------------
bool SampleData::calcPAW(int rep, vector<double>& PAW, vector<double>& targetDepth)
   {
   return false;
/*   PAW.erase(PAW.begin(), PAW.end());

   vector<double> targetLL;

   // calculate target lower limit
   bool Interpolated;
   Interpolated = calcInterpolatedData (charactData->getDepth(),
                                        charactData->getLowerLimit(),
                                        targetDepth,
                                        targetLL);

   if (targetDepth.size() > 0 &&
       targetLL.size() > 0)
      {
      // calculate volumetric water content (%)
      vector<double> volumetric;
      calcVolumetric (rep, volumetric, targetDepth);

      // calculate layer thicknesses
      vector<double> thickness;
      calcThickness(targetDepth, targetDepth, thickness, false);

      // calculate and return PAW in mm
      PAW = subtract(volumetric, targetLL);
      PAW = multiply(PAW, thickness);
      devide_value(PAW, 10.0);
      return Interpolated;
      }
   return false;
*/   }

// ------------------------------------------------------------------
//  Short description:
//    Return nitrate by layer for specified rep (kg/ha)
//    Returns true if data was interpolated.
//    The returned numbers will have been interpolated to match the
//    target depths passed in.

//  Notes:

//  Changes:
//    DPH 20/1/2000

// ------------------------------------------------------------------
bool SampleData::calcNO3(int rep, vector<double>& NO3, vector<double>& targetDepth)
   {
   return false;
/*   vector<double> targetBD;
   vector<double> targetNitrate;

   // calculate target bulk density and nitrate.
   bool Interpolated;
   Interpolated = calcInterpolatedData (charactData->getDepth(),
                                        charactData->getBulkDensity(),
                                        targetDepth,
                                        targetBD);
   Interpolated = calcInterpolatedData (getDepth(),
                                        nitrate[rep-1],
                                        targetDepth,
                                        targetNitrate);

   // need to calculate interpolated Bd's
   if (targetDepth.size() > 0 &&
       targetBD.size() > 0 &&
       targetNitrate.size() > 0)
      {

      vector<double> thickness;
      calcThickness(targetDepth, targetNitrate, thickness, true);

      // calculate volumetric water content (mm)
      NO3 = multiply(targetNitrate, targetBD);
      NO3 = multiply(NO3, thickness);
      devide_value (NO3, 10.0);
      return Interpolated;
      }
   return false;
*/   }
// ------------------------------------------------------------------
//  Short description:
//    Calculate and return volumetric water content by layer averaged for all reps (%)
//    Returns true if data was interpolated.
//    The returned numbers will have been interpolated to match the
//    target depths passed in.

//  Notes:

//  Changes:
//    DPH 20/1/2000

// ------------------------------------------------------------------
bool SampleData::calcAverageVolumetric(vector<double>& averageVolumetric, vector<double>& targetDepth)
   {
   return false;
/*   averageVolumetric.erase(averageVolumetric.begin(), averageVolumetric.end());
   bool Interpolated = false;
   int numReps = getNumReps();
   if (numReps > 0)
      {
      for (int rep = 1; rep <= numReps; rep++)
         {
         vector<double> volumetric;
         Interpolated = calcVolumetric(rep, volumetric, targetDepth);
         if (rep == 1)
            averageVolumetric = volumetric;
         else
            averageVolumetric = add(averageVolumetric, volumetric);
         }
      if (numReps > 0)
         devide_value (averageVolumetric, double(numReps));
      }
   return Interpolated;
*/   }
// ------------------------------------------------------------------
//  Short description:
//     Return a calculated PAW value for all reps.
//    The returned numbers will have been interpolated to match the
//    target depths.

//  Notes:

//  Changes:
//    DPH 13/8/97

// ------------------------------------------------------------------
bool SampleData::calcTotalPAW (double& totalPAW, vector<double>& targetDepth)
   {
   return false;
/*   bool Interpolated = false;
   totalPAW = 0.0;

   int numReps = getNumReps();
   if (numReps > 0)
      {
      for (int rep = 1; rep <= numReps; rep++)
         {
         vector<double> PAW;
         Interpolated = calcPAW(rep, PAW, targetDepth);

         // make sure all paws not negative.
         for (vector<double>::iterator i = PAW.begin();
                                       i != PAW.end();
                                       i++)
            {
            if (*i < 0)
               *i = 0;
            }

         totalPAW += std::accumulate(PAW.begin(), PAW.end(), 0.0);
         }
      totalPAW /= numReps;
      }
   return Interpolated;
*/   }
// ------------------------------------------------------------------
//  Short description:
//     Return a calculated average N03 value (kg/ha).  Return true
//     if data was interpolated.
//    The returned numbers will have been interpolated to match the
//    target depths passed in.

//  Notes:

//  Changes:
//    DPH 13/8/97

// ------------------------------------------------------------------
bool SampleData::calcAverageNO3 (vector<double>& averageNO3, vector<double>& targetDepth)
   {
   return false;
/*   averageNO3.erase(averageNO3.begin(), averageNO3.end());
   bool Interpolated = false;
   int numReps = getNumReps();
   if (numReps > 0)
      {
      int numRealReps = 0;
      for (int rep = 1; rep <= numReps; rep++)
         {
         vector<double> NO3;
         Interpolated = calcNO3(rep, NO3, targetDepth);
         if (rep == 1)
            averageNO3 = NO3;
         else
            averageNO3 = add(averageNO3, NO3);
         if (std::accumulate(NO3.begin(), NO3.end(), 0.0) > 0)
            numRealReps++;
         }
      if (numRealReps > 0)
         devide_value (averageNO3, double(numRealReps));
      }
   return Interpolated;
*/   }

// ------------------------------------------------------------------
//  Short description:
//     Return a calculated average N03 value (ppm).  Return true
//     if data was interpolated.
//    The returned numbers will have been interpolated to match the
//    target depths passed in.

//  Notes:

//  Changes:
//    DPH 13/8/97

// ------------------------------------------------------------------
bool SampleData::calcAverageNO3PPM (vector<double>& averageNO3, vector<double>& targetDepth)
   {
   return false;
/*   averageNO3.erase(averageNO3.begin(), averageNO3.end());
   bool Interpolated = false;
   int numReps = getNumReps();
   if (numReps > 0)
      {
      int numRealReps = 0;
      for (int rep = 1; rep <= numReps; rep++)
         {
         vector<double> NO3;
         Interpolated = calcInterpolatedData (getDepth(),
                                              nitrate[rep-1],
                                              targetDepth,
                                              NO3);
         if (rep == 1)
            averageNO3 = NO3;
         else
            averageNO3 = add(averageNO3, NO3);
         if (std::accumulate(NO3.begin(), NO3.end(), 0.0) > 0)
            numRealReps++;
         }
      if (numRealReps > 0)
         devide_value (averageNO3, double(numRealReps));
      }
   return Interpolated;
*/   }

// ------------------------------------------------------------------
//  Short description:
//    position the specified tree to be the same as the saved position

//  Notes:

//  Changes:
//    DPH 20/1/2000

// ------------------------------------------------------------------
/*void SampleData::restoreTree(TGenericTree* Tree)
   {
   Tree->GotoNode(savedTreePosition);
   }
*/// ------------------------------------------------------------------
//  Short description:
//    save the current tree position

//  Notes:

//  Changes:
//    DPH 20/1/2000

// ------------------------------------------------------------------
/*void SampleData::saveTree(TGenericTree* Tree)
   {
   savedTreePosition = Tree->GetSelectedFQN();
   }
*/
// ------------------------------------------------------------------
//  Short description:
//    Calculate and return an APSIM soil water/nitrogen parameter file.

//  Notes:

//  Changes:
//    DPH 20/1/2000

// ------------------------------------------------------------------
AnsiString SampleData::calcAPSIMFile(void)
   {
   return "";
/*   static double DepthsToUse[7] = {15, 30, 60, 90, 120, 150, 180};
   vector<double> Depth(DepthsToUse, DepthsToUse+7);

   vector<double> MissingValues(Depth.size(), -999999);

   vector<double> LayerNumbers;
   for (unsigned int layer = 1; layer <= Depth.size(); layer++)
      LayerNumbers.push_back(layer);

   vector<double> LL;
   calcInterpolatedData (charactData->getDepth(),
                         charactData->getLowerLimit(),
                         Depth,
                         LL);
   devide_value(LL, 100.0);
   vector<double> DUL;
   calcInterpolatedData (charactData->getDepth(),
                         charactData->getDrainedUpperLimit(),
                         Depth,
                         DUL);
   devide_value(DUL, 100.0);

   vector<double> SAT;
   calcInterpolatedData (charactData->getDepth(),
                         charactData->getSaturation(),
                         Depth,
                         SAT);
   devide_value(SAT, 100.0);

   vector<double> BD;
   calcInterpolatedData (charactData->getDepth(),
                         charactData->getBulkDensity(),
                         Depth,
                         BD);

   vector<double> OC;
   calcInterpolatedData (charactData->getDepth(),
                         charactData->getOrganicCarbon(),
                         Depth,
                         OC);

   vector<double> PH;
   calcInterpolatedData (charactData->getDepth(),
                         charactData->getPH(),
                         Depth,
                         PH);

   vector<double> SW;
   calcAverageVolumetric(SW, Depth);
   devide_value(SW, 100.0);

   // Calculate average raw nitrate (which user enters in mg/kg)
   vector<double> NO3;
   calcAverageNO3PPM (NO3, Depth);

   if (charactData->getDepth().size() > 0 &&
       getDepth().size() > 0)
      {
      std::ostringstream out;
      out.setf(std::ios::fixed, std::ios::floatfield);
      out.precision(3);

      // write soil water profileTable information
      out << "! Soil water profileTable" << std::endl;
      outputToStream(out, "layer", LayerNumbers, 0);
      outputToStream(out, "dlayer", Depth, 3);
      outputToStream(out, "air_dry", MissingValues, 3);
      outputToStream(out, "ll15", MissingValues, 3);
      outputToStream(out, "dul", DUL, 3);
      outputToStream(out, "sat", SAT, 3);
      outputToStream(out, "sw", SW, 3);
      outputToStream(out, "swcon", MissingValues, 3);
      outputToStream(out, "bd", BD, 3);

      // write soil nitrate profileTable information
      out << std::endl << std::endl << "! Soil nitrogen profileTable1" << std::endl;
      outputToStream(out, "layer", LayerNumbers, 0);
      outputToStream(out, "oc", OC, 3);
      outputToStream(out, "ph", PH, 3);
      outputToStream(out, "fbiom", MissingValues, 3);
      outputToStream(out, "finert", MissingValues, 3);
      outputToStream(out, "ureappm", MissingValues, 3);
      out << "#stop" << std::endl;
      outputToStream(out, "no3ppm", NO3, 3);
      outputToStream(out, "nh4ppm", MissingValues, 3);

      // write crop lower limits
      out << std::endl << std::endl << "! Crop lower limit" << std::endl;
      outputToStream(out, "layer", LayerNumbers, 0);
      outputToStream(out, "ll", LL, 3);

      out << std::ends;
      return out.str().c_str();
      }
   else
      return "";
*/   }

// ------------------------------------------------------------------
//  Short description:
//    Output to the specified stream the name and values with the
//    specified number of decimal places.  This routine ensures a
//    column width is used to line up the numbers.

//  Notes:

//  Changes:
//    DPH 20/1/2000

// ------------------------------------------------------------------
void SampleData::outputToStream(std::ostream& out,
                            const AnsiString name,
                            const vector<double>& values,
                            const int numDecPlaces)
   {
/*   out.precision(numDecPlaces);
   out.width(8);
   out << name.c_str() << " = ";
   for (vector<double>::const_iterator valueI = values.begin();
                                       valueI != values.end();
                                       valueI++)
      {
      out.width(8);
      if (*valueI == -999999)
         out << "????";
      else
         out << *valueI;
      }
   out << std::endl;
*/   }
//---------------------------------------------------------------------------
