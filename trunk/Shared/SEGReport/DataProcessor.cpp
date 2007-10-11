//---------------------------------------------------------------------------
#pragma hdrstop
#include "RealSet.h"
#include "Kruskal_wallis.h"
#include <general\pch.h>
#include <vcl.h>

#include "DataProcessor.h"
#include <general\xml.h>
#include <general\stl_functions.h>
#include "ApsimFileReader.h"
#include "Probability.h"
#include "PredObs.h"
#include "XmlFileReader.h"
#include "Filter.h"
#include "Cumulative.h"
#include "Depth.h"
#include "Diff.h"
#include "Frequency.h"
#include "KWTest.h"
#include "REMS.h"
#include "Regression.h"
#include "SOI.h"
#include "Stats.h"
#include "DataContainer.h"
#include "RecordFilter.h"
#include "ReportMacros.h"

//---------------------------------------------------------------------------
// Create a dataprocessor object based on the settings
// passed in. Caller is expected to free the object
// when finished with it.
//---------------------------------------------------------------------------
void processData(DataContainer& parent, const std::string& xml, TDataSet& result)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   string type = doc.documentElement().getName();

   if (Str_i_Eq(type, "ApsimFileReader"))
      processApsimFileReader(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "Probability"))
      processProbability(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "PredObs"))
      processPredObs(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "XmlFileReader"))
      processXmlFileReader(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "Filter"))
      processFilter(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "Cumulative"))
      processCumulative(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "Depth"))
      processDepth(parent, doc.documentElement(), result);
  else if (Str_i_Eq(type, "Diff"))
      processDiff(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "Frequency"))
      processFrequency(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "KWTest"))
      processKWTest(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "REMS"))
      processREMS(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "Regression"))
      processRegression(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "SOI"))
      processSOI(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "Stats"))
      processStats(parent, doc.documentElement(), result);
   else if (Str_i_Eq(type, "RecordFilter"))
      processRecordFilter(parent, doc.documentElement(), result);
   
   }

