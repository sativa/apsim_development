//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ComponentRegistration.h"
#include "TTextForm.h"
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
   TComponentClass classes[11] = {__classid(TText),
                                  __classid(::TShape),
                                  __classid(TQRImage),
                                  __classid(TApsimFileReader),
                                  __classid(TSOI),
                                  __classid(TSEGChart),
                                  __classid(TProbability),
                                  __classid(TREMS),
                                  __classid(TExcel),
                                  __classid(::TFilter),
                                  __classid(TStats)};
   RegisterComponents("Standard", classes, 10);
   }

//---------------------------------------------------------------------------
// Create a form and return a pointer to it for the specified component.
//---------------------------------------------------------------------------
TForm* createComponentUI(TComponent* component, TWinControl* parent)
   {
   if (component->ClassNameIs("TText"))
      {
      TTextForm* form = new TTextForm(NULL);
      form->Parent = parent;
      TText* text = dynamic_cast<TText*> (component);
      form->setComponent(text);
      text->Frame->Style = psClear;
      return form;
      }
   else if (component->ClassNameIs("TShape"))
      {
      TShapeForm* form = new TShapeForm(NULL);
      form->Parent = parent;
      ::TShape* shape = (::TShape*) component;
      form->setComponent(shape);
      shape->Frame->Style = psClear;
      return form;
      }
   else if (component->ClassNameIs("TQRImage"))
      {
      TImageForm* frame = new TImageForm(parent);
      frame->Parent = parent;
      TQRImage* image = dynamic_cast<TQRImage*> (component);
      frame->setComponent(image);
      image->Frame->Style = psClear;
      return frame;
      }
   else if (component->ClassType() == __classid(TApsimFileReader))
      {
      TApsimFileReaderForm* form = new TApsimFileReaderForm(parent);
      form->Parent = parent;
      form->setComponent(dynamic_cast<TApsimFileReader*> (component));
      return form;
      }
   else if (component->ClassType() == __classid(TSOI))
      {
      TSOIForm* form = new TSOIForm(parent);
      form->Parent = parent;
      form->setComponent(dynamic_cast<TSOI*> (component));
      return form;
      }
   else if (component->ClassType() == __classid(TSEGChart))
      {
      TChartForm* form = new TChartForm(parent);
      form->Parent = parent;
      form->setComponent(dynamic_cast<TSEGChart*> (component));
      return form;
      }
   else if (component->ClassType() == __classid(TProbability))
      {
      TProbabilityForm* form = new TProbabilityForm(parent);
      form->Parent = parent;
      form->setComponent(dynamic_cast<TProbability*> (component));
      return form;
      }
   else if (component->ClassType() == __classid(TREMS))
      {
      TREMSForm* form = new TREMSForm(parent);
      form->Parent = parent;
      form->setComponent(dynamic_cast<TREMS*> (component));
      return form;
      }
   else if (component->ClassType() == __classid(TExcel))
      {
      TExcelForm* form = new TExcelForm(parent);
      form->Parent = parent;
      form->setComponent(dynamic_cast<TExcel*> (component));
      return form;
      }
   else if (component->ClassType() == __classid(::TFilter))
      {
      TFilterForm* form = new TFilterForm(parent);
      form->Parent = parent;
      form->setComponent((::TFilter*) (component));
      return form;
      }
   else if (component->ClassType() == __classid(TStats))
      {
      TStatsForm* form = new TStatsForm(parent);
      form->Parent = parent;
      form->setComponent(dynamic_cast<TStats*> (component));
      return form;
      }

   return NULL;
   }

