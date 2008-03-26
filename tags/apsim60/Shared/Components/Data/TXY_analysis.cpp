//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TXY_analysis.h"
#include "TXY_form.h"
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\stristr.h>
#include <ApsimShared\ApsimSettings.h>
using namespace std;
#pragma package(smart_init)

//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TXY_analysis *)
{
   new TXY_analysis(NULL);
}
//---------------------------------------------------------------------------
namespace tXY_analysis
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TXY_analysis)};
      RegisterComponents("APSRU", classes, 0);
   }
}

// ------------------------------------------------------------------
// populate an XYSeries from a string
// ------------------------------------------------------------------
TXY_analysis::XYSeries::XYSeries(const std::string& st)
   {
   vector<string> stringBits;
   splitIntoValues(st, ";", stringBits);
   if (stringBits.size() == 1 || stringBits.size() == 4)
      yname = stringBits[0].c_str();

   if (stringBits.size() == 4)
      {
      seriesType = (SeriesTypes) atoi(stringBits[1].c_str());
      plotOnY2 = (bool) atoi(stringBits[2].c_str());
      isCumulative = (bool) atoi(stringBits[3].c_str());
      }
   }
// ------------------------------------------------------------------
// convert an XYSeries to a string
// ------------------------------------------------------------------
std::string TXY_analysis::XYSeries::toString()
   {
   string st = yname.c_str();
   st += ";";
   st += itoa((int) seriesType);
   st += ";";
   st += itoa((int) plotOnY2);
   st += ";";
   st += itoa((int) isCumulative);
   return st;
   }
// ------------------------------------------------------------------
// populate an XYStatLine from a string
// ------------------------------------------------------------------
TXY_analysis::XYStatLine::XYStatLine(const std::string& st)
   {
   vector<string> stringBits;
   splitIntoValues(st, ";", stringBits);
   if (stringBits.size() == 2)
      {
      dataBlockName = stringBits[0].c_str();
      statType = (StatType) atoi(stringBits[1].c_str());
      }
   }
// ------------------------------------------------------------------
// convert an XYStatLine to a string
// ------------------------------------------------------------------
std::string TXY_analysis::XYStatLine::toString()
   {
   string st = dataBlockName.c_str();
   st += ";";
   st += itoa((int) statType);
   return st;
   }
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 14/7/98

// ------------------------------------------------------------------
__fastcall TXY_analysis::TXY_analysis(TComponent* Owner)
   : TAnalysis(Owner)
   {
   }
// ------------------------------------------------------------------
// Load settings.
// ------------------------------------------------------------------
void TXY_analysis::load()
   {
   TAnalysis::load();
   ApsimSettings settings;

   string XVariableName;
   settings.read(CHART_SETTINGS_KEY + "|XVariableName", XVariableName);
   Field_names_to_analyse->Items->Clear();
   Field_names_to_analyse->Items->Add(XVariableName.c_str());

   vector<string> seriesStrings;
   settings.read(CHART_SETTINGS_KEY + "|SelectedFieldName", seriesStrings);
   for (unsigned i = 0; i != seriesStrings.size(); i++)
      series.push_back(XYSeries(seriesStrings[i]));

   vector<string> statLineStrings;
   settings.read(CHART_SETTINGS_KEY + "|StatLine", statLineStrings);
   for (unsigned i = 0; i != statLineStrings.size(); i++)
      statLines.push_back(XYStatLine(seriesStrings[i]));

   }

// ------------------------------------------------------------------
// Save settings.
// ------------------------------------------------------------------
void TXY_analysis::save()
   {
   TAnalysis::save();
   ApsimSettings settings;

   settings.write(CHART_SETTINGS_KEY + "|XVariableName", string(Field_names_to_analyse->Items->Strings[0].c_str()));
   vector<string> seriesStrings;
   for (unsigned i = 0; i != series.size(); i++)
      seriesStrings.push_back(series[i].toString());
   settings.write(CHART_SETTINGS_KEY + "|SelectedFieldName", seriesStrings);

   vector<string> statLineStrings;
   for (unsigned i = 0; i != statLines.size(); i++)
      statLineStrings.push_back(statLines[i].toString());
   settings.write(CHART_SETTINGS_KEY + "|StatLine", statLineStrings);
   }

// ------------------------------------------------------------------
//  Short description:
//     return true if specified y variable is selected.

//  Changes:
//    DPH 3/8/2001
// ------------------------------------------------------------------
bool TXY_analysis::isYVariableSelected(AnsiString& variableName)
   {
   vector<XYSeries>::iterator i = find(series.begin(),
                                       series.end(),
                                       variableName);
   return (i != series.end());
   }

// ------------------------------------------------------------------
//  Short description:
//     return true if specified y variable is on Y2 axis.

//  Changes:
//    DPH 3/8/2001
// ------------------------------------------------------------------
bool TXY_analysis::isYVariableOnY2(AnsiString& variableName)
   {
   vector<XYSeries>::iterator i = find(series.begin(),
                                       series.end(),
                                       variableName);
   return (i != series.end() && i->plotOnY2);
   }

// ------------------------------------------------------------------
//  Short description:
//     return true if specified y variable is a cumulative variable.

//  Changes:
//    DPH 3/8/2001
// ------------------------------------------------------------------
bool TXY_analysis::isYVariableCumulative(AnsiString& variableName)
   {
   vector<XYSeries>::iterator i = find(series.begin(),
                                       series.end(),
                                       variableName);
   return (i != series.end() && i->isCumulative);
   }
// ------------------------------------------------------------------
// return the series type for the specified variable.
// ------------------------------------------------------------------
SeriesTypes TXY_analysis::getSeriesType(AnsiString& variableName)
   {
   vector<XYSeries>::iterator i = find(series.begin(),
                                       series.end(),
                                       variableName);
   if (i == series.end())
      return markers;
   else
      return i->seriesType;
   }         
// ------------------------------------------------------------------
//  Short description:
//     return details for specified y variable

//  Changes:
//    DPH 3/8/2001
// ------------------------------------------------------------------
void TXY_analysis::getSeriesDetails
   (unsigned int seriesIndex, AnsiString& name, bool& plotOnY2,
    bool& isCumulative, SeriesTypes& seriesType)
   {
   if (seriesIndex < series.size())
      {
      name = series[seriesIndex].yname;
      plotOnY2 = series[seriesIndex].plotOnY2;
      isCumulative = series[seriesIndex].isCumulative;
      seriesType = series[seriesIndex].seriesType;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     simple return the sourceDataset

//  Notes:

//  Changes:
//    DPH 30/7/98

// ------------------------------------------------------------------
void TXY_analysis::calcAndStoreRecords(void)
   {
   beginStoringData();
   if (Field_names_to_analyse->Items->Count > 0)
      {
      vector<string> fieldsToKeep;
      fieldsToKeep.push_back(Field_names_to_analyse->Items->Strings[0].c_str());
      for (vector<XYSeries>::iterator seriesI = series.begin();
                                      seriesI != series.end();
                                      seriesI++)
          fieldsToKeep.push_back(seriesI->yname.c_str());

      // keep a year field
      vector<string> fieldNames;
      sourceDataset->getFieldNames(fieldNames);
      for (vector<string>::iterator fieldI = fieldNames.begin();
                                    fieldI != fieldNames.end();
                                    fieldI++)
         {
         if (stristr((char*) fieldI->c_str(), "year") != NULL)
            {
            if (find(fieldsToKeep.begin(), fieldsToKeep.end(), *fieldI) == fieldsToKeep.end())
               fieldsToKeep.push_back(*fieldI);
            break;
            }
         }
         
      // tell the destination apstable (us) to get ready for first data block.
      first();

      // get all data.
      bool ok = sourceDataset->first();
      while (ok)
         {
         for (vector<string>::iterator fieldI = fieldsToKeep.begin();
                                       fieldI != fieldsToKeep.end();
                                       fieldI++)
            {
            // get our field values.
            vector<string> values;
            sourceDataset->fieldAsStringArray(*fieldI, values);
            storeStringArray(*fieldI, values);
            addField(*fieldI);
            }

         // add any stat lines for this datablock
         addStatLineDataFor(sourceDataset->getDataBlockName());

         // copy pivot fields from the source dataset.
         copyPivotsFrom(*sourceDataset);

         // goto next pivot series in source dataset.
         ok = sourceDataset->next();

         // tell the destination apstable to get ready for next data.
         next();
         }
      }
   endStoringData();
   }

// ------------------------------------------------------------------
// Return a string that is the same as the specified string but
// with units removed.
// ------------------------------------------------------------------
AnsiString stripUnits(AnsiString st)
   {
   int posUnits = st.Pos("(");
   if (posUnits != 0)
      return st.SubString(1, posUnits-1);
   else
      return st;
   }

// ------------------------------------------------------------------
// Add stat lines for the specified datablock.
// ------------------------------------------------------------------
void TXY_analysis::addStatLineDataFor(const string& dataBlockName)
   {
   AnsiString xName = Field_names_to_analyse->Items->Strings[0];
   for (vector<XYStatLine>::iterator statI = statLines.begin();
                                     statI != statLines.end();
                                     statI++)
      {
      if (statI->dataBlockName == dataBlockName.c_str())
         {
         // Get the x values and calculate the minimum and maximum x value.
         // This will be used for plotting the start and end point of
         // the line on the chart.
         vector<double> xValues;
         for (vector<TAPSRecord>::const_iterator i = sourceDataset->begin();
                                                 i != sourceDataset->end();
                                                 i++)
            {
            string value = (*i).getFieldValue(xName.c_str());
            if (value != "")
               xValues.push_back(atof(value.c_str()));
            }
         double minX = *min_element(xValues.begin(), xValues.end());
         double maxX = *max_element(xValues.begin(), xValues.end());

         for (vector<XYSeries>::iterator seriesI = series.begin();
                                         seriesI != series.end();
                                         seriesI++)
            {
            // get the Y values and calculate the stat.
            vector<double> yValues;
            for (vector<TAPSRecord>::const_iterator i = sourceDataset->begin();
                                                    i != sourceDataset->end();
                                                    i++)
               {
               string value = (*i).getFieldValue(seriesI->yname.c_str());
               if (value != "")
                  yValues.push_back(atof(value.c_str()));
               }
            double statValue = calcStatValue(yValues, statI->statType);

            // work out a field name for our 'statistic' column.
            AnsiString statName = statTypeToName(statI->statType);
            AnsiString statisticName = stripUnits(seriesI->yname) + ": ";
            statisticName += dataBlockName.c_str();
            statisticName += ": " + statName + "=";
            statisticName += ftoa(statValue, 2).c_str();
            AnsiString statNameFieldName = "statname_" + statName + "_" + seriesI->yname;

            // work out the field names we're going to write to.
            AnsiString statXFieldName = "stat_" + statName + "_" + xName;
            AnsiString statYFieldName = "stat_" + statName + "_" + seriesI->yname;

            // write our stat values to field.
            vector<TAPSRecord>::iterator recordI
               = const_cast<vector<TAPSRecord>::iterator> (begin());
            recordI->setFieldValue(statXFieldName.c_str(), FloatToStr(minX).c_str());
            recordI->setFieldValue(statYFieldName.c_str(), FloatToStr(statValue).c_str());
            recordI->setFieldValue(statNameFieldName.c_str(), statisticName.c_str());
            recordI++;
            recordI->setFieldValue(statXFieldName.c_str(), FloatToStr(maxX).c_str());
            recordI->setFieldValue(statYFieldName.c_str(), FloatToStr(statValue).c_str());
            recordI->setFieldValue(statNameFieldName.c_str(), statisticName.c_str());

            addField(statXFieldName.c_str());
            addField(statYFieldName.c_str());
            addField(statNameFieldName.c_str());
            }
         }
      }
   }
// ------------------------------------------------------------------
// convert a stattype to a statname.
// ------------------------------------------------------------------
AnsiString TXY_analysis::statTypeToName(StatType type) const
   {
   AnsiString statisticName;
   if (type == mean)
      statisticName += "Mean";
   else
      {
      statisticName = IntToStr((int)type*10);
      statisticName += "%";
      }
   return statisticName;
   }
// ------------------------------------------------------------------
// Calculate a stat value for the specified points.
// ------------------------------------------------------------------
double TXY_analysis::calcStatValue(vector<double>& yValues, StatType statType)
   {
   if (statType == mean)
      return Calculate_mean(yValues);
   else
      {
      int percentile = (int)statType * 10; 
      return Calculate_percentile(yValues, false, percentile);
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      create and return a pointer to an analysis form.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TAPSTable_form* TXY_analysis::createPropertiesForm()
   {
   return new TXY_form(Application);
   }


