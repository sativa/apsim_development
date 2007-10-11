//---------------------------------------------------------------------------
#ifndef DataProcessorH
#define DataProcessorH

#include <db.hpp>

class DataContainer;
//---------------------------------------------------------------------------
// This function performs some data processing based on the specified
// parent and xml.
//---------------------------------------------------------------------------
void processData(DataContainer& parent, const std::string& xml, TDataSet& result);

#endif
