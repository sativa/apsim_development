//---------------------------------------------------------------------------
#ifndef ProbabilityH
#define ProbabilityH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// this function creates probability distributions from source data.
//---------------------------------------------------------------------------
void processProbability(DataContainer& parent,
                        const XMLNode& properties,
                        TDataSet& result);

#endif
