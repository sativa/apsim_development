//---------------------------------------------------------------------------
// Small class used to determine if the data in clipboard is sample or soil
//---------------------------------------------------------------------------
#include <general\pch.h>
#pragma hdrstop

#include <general\xml.h>
#include <general\math_functions.h>
#include <boost\lexical_cast.hpp>
#include <numeric>
#include "Clipboardtester.h"
using namespace std;
using namespace boost;

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Clipboardtester::Clipboardtester(const string& xml)
   {
   doc = new XMLDocument(xml, XMLDocument::xmlContents);
   }

Clipboardtester::Clipboardtester(const XMLNode& xml)
   {
      doc = new XMLDocument(xml.write(), XMLDocument::xmlContents);

//      XMLNode childNode = doc->documentElement();
//      doc = new XMLDocument(
//      xml;
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
Clipboardtester::~Clipboardtester(void)
   {
   delete doc;
   }



//---------------------------------------------------------------------------

#pragma package(smart_init)
