//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TText.h"
#include <general\stringtokenizer.h>
#include <general\vcl_functions.h>
#pragma package(smart_init)
using namespace std;
//---------------------------------------------------------------------------
__fastcall TText::TText(TComponent* Owner)
   : TgtQRMemo(Owner)
   {
   }
//---------------------------------------------------------------------------
// Component has been loaded from stream - setup all data source links.
//---------------------------------------------------------------------------
void __fastcall TText::Loaded(void)
   {
   TgtQRMemo::Loaded();
   refresh();
   }
//---------------------------------------------------------------------------
// get the text property.
//---------------------------------------------------------------------------
AnsiString __fastcall TText::getText(void)
   {
   return contentsWithMacros;
   }
//---------------------------------------------------------------------------
// set the text property.
//---------------------------------------------------------------------------
void __fastcall TText::setText(AnsiString text)
   {
   contentsWithMacros = text;
   refresh();
   trapSourceDataRefresh();
   }
//---------------------------------------------------------------------------
// Return alignment as a string.
//---------------------------------------------------------------------------
AnsiString __fastcall TText::getAlignment(void)
   {
   if (Alignment == taLeftJustify)
      return "Left";
   else if (Alignment == taCenter)
      return "Centre";
   else
      return "Right";
   }
//---------------------------------------------------------------------------
// Set alignment
//---------------------------------------------------------------------------
void __fastcall TText::setAlignment(AnsiString alignmentString)
   {
   if (alignmentString == "Left")
      Alignment = taLeftJustify;
   else if (alignmentString == "Centre")
      Alignment = taCenter;
   else
      Alignment = taRightJustify;
   }
//---------------------------------------------------------------------------
// refresh the control
//---------------------------------------------------------------------------
void TText::refresh(void)
   {
   Lines->Text = macros.doReplacement(Owner, contentsWithMacros);
   Paint();
   }

//---------------------------------------------------------------------------
// trap the dataset onDataRefresh events so that we can update our macros.
//---------------------------------------------------------------------------
void TText::trapSourceDataRefresh(void)
   {
   // remove ourself from all source data lists.
   for (unsigned i = 0; i != sourceNames.size(); i++)
      {
      TSEGTable* source = getComponent<TSEGTable>(Owner, sourceNames[i].c_str());
      if (source != NULL)
         source->removeDataChangeSubscription(Name.c_str());
      }

   // find data component on owner.
   TForm* data = getComponent<TForm>(Owner, "data");

   // add ourself to all source components.
   macros.getReferencedComponents(sourceNames);
   for (unsigned i = 0; i != sourceNames.size(); i++)
      {
      TSEGTable* source = getComponent<TSEGTable>(data, sourceNames[i].c_str());
      if (source != NULL)
         source->addDataChangeSubscription(Name + ".afterDataRefresh");
      }
   }

//---------------------------------------------------------------------------
// One of the source datasets is now open - refresh our macros.
//---------------------------------------------------------------------------
void __fastcall TText::afterDataRefresh(TDataSet* dataset)
   {
   refresh();
   }


