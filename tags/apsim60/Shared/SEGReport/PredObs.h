//---------------------------------------------------------------------------
#ifndef PredObsH
#define PredObsH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// this function does predicted / observed data matching.
//---------------------------------------------------------------------------
void processPredObs(DataContainer& parent,
                    const XMLNode& properties,
                    TDataSet& result);

#endif