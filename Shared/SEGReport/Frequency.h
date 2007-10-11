//---------------------------------------------------------------------------
#ifndef FrequencyH
#define FrequencyH

#include <db.hpp>
class XMLNode;
class DataContainer;

//---------------------------------------------------------------------------
// Creates a frequency distribution from source data.
//---------------------------------------------------------------------------
void processFrequency(DataContainer& parent,
                      const XMLNode& properties,
                      TDataSet& result);
#endif
