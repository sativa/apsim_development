#include <general\pch.h>
#include <vcl.h>
#include <boost/function.hpp>
#pragma hdrstop

#include <math.h>
#include <string>
#include <strstream>
#include <iomanip.h>

#include <general/string_functions.h>
#include <general/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>
#include "ConverterBase.h"


#pragma package(smart_init)
using namespace std;


#define singleArrayTypeDDML \
   "<type  array=\"T\" kind=\"single\"/>"


// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ConverterBase::ConverterBase(protocol::Component *s)
   {
      system = s;
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
ConverterBase::~ConverterBase(void)
   {
   }
