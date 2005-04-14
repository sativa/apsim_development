//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "FiltAddIn.h"
#include "TFilterForm.h"
#include <general\string_functions.h>
#include <parseexpr.hpp>

#pragma resource "Filter.res"
using namespace std;
// ------------------------------------------------------------------
//  Short description:
//    Exported function for created an instance of this add-in

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
extern "C" ToolBarAddInBase* _export __stdcall createToolBarAddIn(const string& parameters)
   {
   return new FiltAddIn(parameters);
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 24/9/98

// ------------------------------------------------------------------
FiltAddIn::FiltAddIn(const string& parameters)
   {
   needsUpdating = false;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 5/4/01
//    DAH 30/7/01
//       added commands to remove button and glyph from toolbar and imageindex
//       respectively, and to delete the button and glyph

// ------------------------------------------------------------------
FiltAddIn::~FiltAddIn(void)
   {
   int pos = filterButton->ImageIndex;
   Toolbar->RemoveControl(filterButton);
   delete filterButton;
   Toolbar->Images->Delete(pos);
   delete glyph;
   }

// ------------------------------------------------------------------
//  Short description:
//    Add buttons etc. to the ToolBar passed in.

//  Notes:

//  Changes:
//    DPH 4/4/01
//    DAH 30/7/01    changed filterbutton and glyph to be a member variables so we
//                   can delete them later. Kept a record of the toolbar passed in

// ------------------------------------------------------------------
void FiltAddIn::decorateToolBar(TToolBar* toolbar)
   {
   Toolbar = toolbar;
   filterButton = new TToolButton(toolbar);
   filterButton->Left = toolbar->Width; // ensures button goes at right end of row
   filterButton->Parent = toolbar;

   glyph = new Graphics::TBitmap;
   glyph->LoadFromResourceName((unsigned int) HInstance, "FILTER_BITMAP");
   glyph->Transparent = true;

   filterButton->ImageIndex = toolbar->Images->Add(glyph, NULL);
   filterButton->Hint = "Apply filter";
   filterButton->OnClick = buttonClick;
   }

// ------------------------------------------------------------------
//  Short description:
//    Add buttons etc. to the ToolBar passed in.

//  Notes:

//  Changes:
//    DPH 4/4/01

// ------------------------------------------------------------------
void __fastcall FiltAddIn::buttonClick(TObject* Sender)
   {
   if (working->first())
      {
      fieldValues.clearFields();
      fieldValues.setupFields(*working->begin());
      }
   FilterForm = new TFilterForm(Application->MainForm);
   FilterForm->filterAddIn = this;
   fieldValues.giveFieldsToFilterBox(*FilterForm->FilterBox1);
   fieldValues.giveFieldsToFilterBox(*FilterForm->FilterBox2);
   fieldValues.giveFieldsToFilterBox(*FilterForm->FilterBox3);
   fieldValues.giveFieldsToFilterBox(*FilterForm->FilterBox4);
   fieldValues.giveFieldsToFilterBox(*FilterForm->FilterBox5);
   needsUpdating = (FilterForm->ShowModal() == mrOk);
   delete FilterForm;
   }

// ------------------------------------------------------------------
//  Short description:
//    given the source data object, and the list of user selected
//    scenarios, perform all calculations and store all new data
//    in the returned TAPSTable.

//  Changes:
//    DPH 5/2/98
//    dph 8/12/1999 replaced call to get_desciptor_field_names with a
//                  call to get_name.  We no longer want specially created
//                  descriptor names. c227,d308
//    DAH 10/8/2000 saved and restored cursor
//    dph 5/4/2001 pulled from the old TSelected_simulations.cpp and modified
//    DAH 30/7/01    removed needsUpdating condition - this flag is essentially
//                   used externally to work out whether or not to call doCalculations
//                   for each toolbar addin.

// ------------------------------------------------------------------
void FiltAddIn::doCalculations(TAPSTable& data)
   {
   if (/*needsUpdating &&*/ filters.size() > 0)
      {
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      // Create a destination table and set it up with all fields.
      // Later this table will replace the one passed in.
      TAPSTable* destData = new TAPSTable(NULL);
      destData->beginStoringData();
      vector<string> fieldNames;
      data.getFieldNames(fieldNames);
      for (vector<string>::iterator i = fieldNames.begin();
                                    i != fieldNames.end();
                                    i++)
         destData->addField(*i);
      destData->copyPivotsFrom(data);

      // create an expression parser and give all fields to it.
      TExpressionParser* expressionParser = new TExpressionParser();
      fieldValues.giveFieldsToParser(*expressionParser);

      // loop through all filters.
      destData->first();
      for (vector<string>::iterator filterI = filters.begin();
                                    filterI != filters.end();
                                    filterI++)
         {
         expressionParser->ClearExpressions();
         string filterMinusBraces = *filterI;
         Replace_all(filterMinusBraces, "[", "");
         Replace_all(filterMinusBraces, "]", "");
         expressionParser->AddExpression(filterMinusBraces.c_str());

         bool ok = data.first();
         while (ok)
            {
            // work out the current data block name (ie "Simulation" field name)
            string blockName = data.getDataBlockName();
            blockName = blockName + ";filter=" + filterMinusBraces;

            for (vector<TAPSRecord>::const_iterator recI = data.begin();
                                                    recI != data.end();
                                                    recI++)
               {
               TAPSRecord rec = *recI;
               fieldValues.updateFieldValues(rec);

               // if the evaluated expression returns true then we need to keep
               // this record.
               if (expressionParser->EvaluateCurrent() == 1)
                  {
                  rec.setFieldValue("Filter", *filterI);
                  rec.setFieldValue("Simulation", blockName);
                  destData->storeRecord(rec);
                  }
               }

            ok = data.next();
            destData->next();
            }
         }
      data.beginStoringData();
      data.storeData(*destData);
      data.markFieldAsAPivot("Filter");
      data.endStoringData();

      delete destData;
      delete expressionParser;
      needsUpdating = false;

      Screen->Cursor = savedCursor;
      }
   }


