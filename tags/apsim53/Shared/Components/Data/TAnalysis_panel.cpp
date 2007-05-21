//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAnalysis_panel.h"
#include <editchar.hpp>
#pragma package(smart_init)
#pragma link "EditChar"
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TAnalysis_panel *)
{
   new TAnalysis_panel(NULL);
}
//---------------------------------------------------------------------------
namespace Tanalysis_panel
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TAnalysis_panel)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
__fastcall TAnalysis_panel::TAnalysis_panel(TComponent* Owner)
   : TAuto_size_panel(Owner)
   {
   FSource_data = NULL;
   Analyses[0] = NULL;
   Analysis_charts[0] = NULL;
   FColour_background = true;
   }


// ------------------------------------------------------------------
//  Short description:
//    set the source data

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void __fastcall  TAnalysis_panel::Set_source_data (TAPSTable* source)
   {
   FSource_data = source;
   for (int i = 0; i < MAX_ANALYSES && Analyses[i] != NULL; i++)
      Analyses[i]->sourceDataset = Source_data;
   for (int i = 0; i < MAX_CHARTS && Analysis_charts[i] != NULL; i++)
      {
      if (Analyses[i] == NULL)
         Analysis_charts[i]->APSTable = Source_data;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    return a pointer to the destination dataset.

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
TAPSTable* __fastcall  TAnalysis_panel::Get_destination_data (void)
   {
   return Analyses[0]; 
   }

// ------------------------------------------------------------------
// Return the main field name to caller.
// ------------------------------------------------------------------
AnsiString __fastcall TAnalysis_panel::Get_main_field_name()
   {
   if (Analyses[0] != NULL)
      return Analyses[0]->Field_names_to_analyse->Items->Strings[0];
   else
      return "";
   }
// ------------------------------------------------------------------
//  Short description:
//    refresh all analysis objects

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void TAnalysis_panel::Refresh_analysis_objects (void)
   {
   for (int i = 0; i < MAX_ANALYSES && Analyses[i] != NULL; i++)
      Analyses[i]->refresh();
   }

// ------------------------------------------------------------------
//  Short description:
//    refresh all chart objects

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void TAnalysis_panel::Refresh_chart_objects (void)
   {
   if (FSource_data != NULL)
      {
      for (int i = 0; i < MAX_CHARTS && Analysis_charts[i] != NULL; i++)
         {
         if (Analysis_charts[i]->Visible)
            Analysis_charts[i]->Refresh();
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    delete all objects

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void TAnalysis_panel::Delete_objects (void)
   {
   for (int i = 0; i < MAX_ANALYSES && Analyses[i] != NULL; i++)
      {
      delete Analyses[i];
      Analyses[i] = NULL;
      }
   for (int i = 0; i < MAX_CHARTS && Analysis_charts[i] != NULL; i++)
      {
      delete Analysis_charts[i];
      Analysis_charts[i] = NULL;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    initialise object.

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void TAnalysis_panel::Init (void)
   {
   Delete_objects();
   Create_objects();

   // hook all objects together.
   for (int i = 0; i < MAX_ANALYSES && Analyses[i] != NULL; i++)
      Analyses[i]->sourceDataset = Source_data;

   for (int i = 0; i < MAX_CHARTS && Analysis_charts[i] != NULL; i++)
      {
      if (Analyses[i] == NULL)
         Analysis_charts[i]->APSTable = Source_data;
      else
         Analysis_charts[i]->APSTable = Analyses[i];
      Analysis_charts[i]->Parent = this;
      }

   }

// ------------------------------------------------------------------
//  Short description:
//    edit object.

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
bool TAnalysis_panel::Edit (void)
   {
   bool ok = true;
   for (int i = 0; i < MAX_ANALYSES && Analyses[i] != NULL && ok; i++)
      {
      if (i > 0)
         Analyses[i-1]->refresh();
      ok = Analyses[i]->edit();
      }
   for (int i = 0; i < MAX_CHARTS && Analysis_charts[i] != NULL && ok; i++)
      ok = Analysis_charts[i]->edit();

   return ok;
   }

// ------------------------------------------------------------------
//  Short description:
//    refresh object.

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void TAnalysis_panel::Refresh (void)
   {
   Refresh_analysis_objects();
   Refresh_chart_objects();
   Set_large_fonts(FLarge_fonts);
   Resize();
   }

// ------------------------------------------------------------------
//  Short description:
//    let user edit a chart.

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void TAnalysis_panel::Edit_chart(void)
   {
   if (Analysis_charts[1] == NULL)
      ::EditChart (Application->MainForm, Analysis_charts[0]);
   else
      {
      TMultiStringList* Form = new TMultiStringList;

      for (int i = 0; i < MAX_CHARTS && Analysis_charts[i] != NULL; i++)
         Form->PossibleItems->Add(Analysis_charts[i]->Title->Text->Strings[0]);
      Form->Edit();
      if (Form->Items->Count == 1)
         {
         for (int i = 0; i < MAX_CHARTS && Analysis_charts[i] != NULL; i++)
            {
            if (Analysis_charts[i]->Title->Text->Strings[0] == Form->Items->Strings[0])
               ::EditChart (Application->MainForm, Analysis_charts[i]);
            }
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    copy the panel contents to the clipboard.

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void TAnalysis_panel::Copy_to_clipboard (bool With_background)
   {
   // setup the metafile
   Graphics::TMetafile* metafile = new TMetafile;
   metafile->Width  = Width;
   metafile->Height = Height;
   metafile->Enhanced = true;
   Graphics::TMetafileCanvas* metafile_canvas = new Graphics::TMetafileCanvas (metafile, 0);

   // setup the bitmap
   Graphics::TBitmap* bitmap = new Graphics::TBitmap();
   bitmap->Width = ClientWidth;
   bitmap->Height = ClientHeight;
   bitmap->Canvas->Lock();
   try
      {
      for (int i = 0; i < MAX_CHARTS && Analysis_charts[i] != NULL; i++)
         {
         if (!With_background)
            Analysis_charts[i]->Draw_background = false;

         // write chart to bitmap.
         Analysis_charts[i]->PaintTo(bitmap->Canvas->Handle, Analysis_charts[i]->Left, Analysis_charts[i]->Top);

         // write chart to metafile.
         Analysis_charts[i]->DrawToMetaCanvas(metafile_canvas, Analysis_charts[i]->BoundsRect);

         if (!With_background)
            Analysis_charts[i]->Draw_background = true;

         }
      }
   catch (...)
      {
      bitmap->Canvas->Unlock();
      }

	try
    	{
      delete metafile_canvas;

		Clipboard()->Open();
      Clipboard()->Assign(bitmap);
      Clipboard()->Assign(metafile);
      Clipboard()->Close();
    	}
   catch(...)
    	{
      }
   delete bitmap;
   delete metafile;
   }

// ------------------------------------------------------------------
//  Short description:
//    setup the large fonts property

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void __fastcall TAnalysis_panel::Set_large_fonts(bool large_fonts)
   {
   if (FLarge_fonts != large_fonts)
      {
      FLarge_fonts = large_fonts;

      for (int i = 0; i < MAX_CHARTS && Analysis_charts[i] != NULL; i++)
         Analysis_charts[i]->Large_fonts = large_fonts;

      Refresh_chart_objects();
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    set the colour_background property.

//  Notes:

//  Changes:
//    DPH 9/12/99 C232

// ------------------------------------------------------------------
void __fastcall TAnalysis_panel::Set_colour_background(bool Colour_background)
   {
   if (FColour_background != Colour_background)
      {
      FColour_background = Colour_background;

      for (int i = 0; i < MAX_CHARTS && Analysis_charts[i] != NULL; i++)
         Analysis_charts[i]->Colour_background = FColour_background;

      Refresh_chart_objects();
      }
   }
