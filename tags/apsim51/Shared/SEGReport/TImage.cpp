//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TImage.h"
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall ::TImage::TImage(TComponent* Owner)
   : TgtQRImage(Owner)
   {
   FImageAsLink = false;
   }
//---------------------------------------------------------------------------
void __fastcall ::TImage::Loaded(void)
   {
   TgtQRImage::Loaded();
   if (ImageAsLink && FileExists(FileName))
      Picture->LoadFromFile(FileName);
   else
      Picture->LoadFromFile("");
   }
//---------------------------------------------------------------------------
void __fastcall ::TImage::SetLink(bool link)
   {
   FImageAsLink = true;
   }
//---------------------------------------------------------------------------
void __fastcall ::TImage::SetFileName(AnsiString FileName)
   {
   if (FileName != FFileName && FileExists(FileName))
      {
      Picture->LoadFromFile(FileName);
      FFileName = FileName;
      }
   }

