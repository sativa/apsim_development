//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ComponentRegistration.h"
#include "TRichTextForm.h"
#include "TImageForm.h"
#include "TApsimFileReaderForm.h"
#include "TSOIForm.h"
#include "TChartForm.h"
#include "TProbabilityForm.h"
#include "TREMSForm.h"
#include "TExcelForm.h"
#include "TFilterForm.h"

//---------------------------------------------------------------------------
#pragma resource "*.res"

#pragma package(smart_init)

//---------------------------------------------------------------------------
// Called by SEG report to register all components.
// These components will appear on the
// form designer palette allowing the user to drop them on the report.
//---------------------------------------------------------------------------
void RegisterComponents(void)
   {
   TComponentClass classes[9] = {__classid(TRichText),
                                 __classid(TQRImage),
                                 __classid(TApsimFileReader),
                                 __classid(TSOI),
                                 __classid(TSEGChart),
                                 __classid(TProbability),
                                 __classid(TREMS),
                                 __classid(TExcel),
                                 __classid(::TFilter)};
   RegisterComponents("Standard", classes, 8);
   }

//---------------------------------------------------------------------------
// Create a form and return a pointer to it for the specified component.
//---------------------------------------------------------------------------
TForm* createComponentUI(TComponent* component, TWinControl* parent)
   {
   if (component->ClassNameIs("TRichText"))
      {
      TRichTextForm* form = new TRichTextForm(NULL);
      form->Parent = parent;
      TRichText* richText = dynamic_cast<TRichText*> (component);
      form->setComponent(richText);
      richText->Frame->Style = psClear;
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

   return NULL;
   }

