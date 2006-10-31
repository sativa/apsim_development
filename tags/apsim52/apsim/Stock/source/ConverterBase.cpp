#include <general\pch.h>
#include <vcl.h>
#include <boost/function.hpp>
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
