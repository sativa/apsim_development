//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Factor.h"
#include <general\stringTokenizer.h>

#pragma package(smart_init)
using namespace std;

//---------------------------------------------------------------------------
// return the state of this factor as a string.
//---------------------------------------------------------------------------
string Factor::getState(void) const
   {
   return name + "=" + value;
   }

//---------------------------------------------------------------------------
// setup this factor from the state string passed in.
//---------------------------------------------------------------------------
void Factor::setState(const std::string& state)
   {
   StringTokenizer tokenizer(state, "=");
   name = tokenizer.nextToken();
   value = tokenizer.nextToken();
   }


