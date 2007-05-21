//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAuto_size_panel.h"
#pragma package(smart_init)
#pragma resource "*.res"
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TAuto_size_panel *)
{
   new TAuto_size_panel(NULL);
}
//---------------------------------------------------------------------------
namespace Tauto_size_panel
{
   void __fastcall PACKAGE Register()
   {                                  
      TComponentClass classes[1] = {__classid(TAuto_size_panel)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
__fastcall TAuto_size_panel::TAuto_size_panel(TComponent* Owner)
   : TPanel(Owner)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//      panel is resizing - position all components.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void __fastcall TAuto_size_panel::Resize(void)
   {
   Position_all_components();
   }

// ------------------------------------------------------------------
//  Short description:
//      panel is resizing - position all components.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void __fastcall TAuto_size_panel::Position_all_components()
   {
   if (ControlCount > 0 && Parent != NULL)
      {
      // how many control are we dealing with here?  Exclude all child forms.
      int count = 0;
      for (int control = 0; control < ControlCount; control++)
         {
         if (dynamic_cast<TForm*> (Controls[control]) == NULL)
            count++;
         }

      if (count > 0)
         {
         int Height_of_each_control = ClientHeight / count;
         for (int control = 0; control < ControlCount; control++)
            {
            if (dynamic_cast<TForm*> (Controls[control]) == NULL &&
                Controls[control]->Parent != NULL  )
               {
               Controls[control]->Left = 0;
               Controls[control]->Top = control * Height_of_each_control;
               Controls[control]->Width = ClientWidth;
               Controls[control]->Height = Height_of_each_control;
               }
            }
         }
      }
   }
