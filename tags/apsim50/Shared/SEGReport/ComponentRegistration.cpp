//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ComponentRegistration.h"
#include "TImageForm.h"
#include "TApsimFileReaderForm.h"
#include "TSOIForm.h"
#include "TChartForm.h"
#include "TProbabilityForm.h"
#include "TREMSForm.h"
#include "TExcelForm.h"
#include "TFilterForm.h"
#include "TStatsForm.h"
#include "TeeEditPro.hpp"
#include "TDecileFunction.h"
#include "TShapeForm.h"
#include "TTextForm.h"
#include "TXYForm.h"
#include "TCumulative.h"
#include "TDiff.h"
#include "TDiffForm.h"
//---------------------------------------------------------------------------
#pragma resource "*.res"
#pragma package(smart_init)
#pragma link "TeeEditPro"

//---------------------------------------------------------------------------
// Called by SEG report to register all components.
// These components will appear on the
// form designer palette allowing the user to drop them on the report.
//---------------------------------------------------------------------------
AnsiString DecileDescription = "Decile function";
void RegisterComponents(void)
   {
   RegisterTeeBasicFunction(__classid(TDecileFunction), &DecileDescription);
   TComponentClass standardClasses[5] = {__classid(TText),
                                  __classid(::TShape),
                                  __classid(::TImage),
                                  __classid(::TGraph),
                                  __classid(::TXYGraph)};
   RegisterComponents("Standard", standardClasses, 4);
   TComponentClass dataClasses[9] = {__classid(TApsimFileReader),
                                  __classid(TSOI),
                                  __classid(TProbability),
                                  __classid(TREMS),
                                  __classid(TExcel),
                                  __classid(::TFilter),
                                  __classid(TStats),
                                  __classid(TCumulative),
                                  __classid(TDiff)};
   RegisterComponents("Data", dataClasses, 8);
   }

//---------------------------------------------------------------------------
// Create a form and return a pointer to it for the specified component.
//---------------------------------------------------------------------------
TForm* createComponentUI(TComponent* component, TWinControl* parent,
                         bool showAdvanced)
   {
   TPropertyForm* form;

   if (component->ClassType() == __classid(TText))
      form = new TTextForm(NULL);
   else if (component->ClassType() == __classid(::TShape))
      form = new TShapeForm(NULL);
   else if (component->ClassType() == __classid(::TImage))
      form = new TImageForm(parent);
   else if (component->ClassType() == __classid(TApsimFileReader))
      form = new TApsimFileReaderForm(parent);
   else if (component->ClassType() == __classid(TSOI))
      form = new TSOIForm(parent);
   else if (component->ClassType() == __classid(::TGraph))
      form = new TChartForm(parent);
   else if (component->ClassType() == __classid(::TXYGraph))
      form = new TXYForm(parent);
   else if (component->ClassType() == __classid(TProbability))
      form = new TProbabilityForm(parent);
   else if (component->ClassType() == __classid(TREMS))
      form = new TREMSForm(parent);
   else if (component->ClassType() == __classid(TExcel))
      form = new TExcelForm(parent);
   else if (component->ClassType() == __classid(::TFilter))
      form = new TFilterForm(parent);
   else if (component->ClassType() == __classid(TStats))
      form = new TStatsForm(parent);
   else if (component->ClassType() == __classid(TDiff))
      form = new TDiffForm(parent);
   else
      form = new TPropertyForm(parent);

   form->Parent = parent;
   form->setup(component, showAdvanced);
   return form;
   }

