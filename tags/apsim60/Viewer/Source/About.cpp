//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "About.h"
String aVersion(void);

//---------------------------------------------------------------------
#pragma resource "*.dfm"
TAboutBox *AboutBox;
//---------------------------------------------------------------------
__fastcall TAboutBox::TAboutBox(TComponent* AOwner)
	: TForm(AOwner)
{
}
//---------------------------------------------------------------------

void __fastcall TAboutBox::FormShow(TObject *Sender)
   {
   VersionLabel->Caption = aVersion();   
   }
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// Function: String aVersion(void)
// Description: Retrieves and returns the version number of the application
//---------------------------------------------------------------------------

String aVersion(void)
   {
   String ExeName = Application->ExeName;
   AnsiString version = "No Version Information Available";
   unsigned long dummy;
   int size = GetFileVersionInfoSize(ExeName.c_str(), &dummy);
   if(!size)
      {
      return version;
      }
   unsigned long *buffer = new unsigned long[size];

   GetFileVersionInfo(ExeName.c_str(), NULL, size, buffer);

   void *lpBuffer;
   unsigned int dwBytes;
   VerQueryValue(buffer, "\\", &lpBuffer, &dwBytes);

   VS_FIXEDFILEINFO *lpvsinfo = (VS_FIXEDFILEINFO *)lpBuffer;
   int MajorVersion = HIWORD(lpvsinfo->dwFileVersionMS);
   int MinorVersion = LOWORD(lpvsinfo->dwFileVersionMS);
   int Release = HIWORD(lpvsinfo->dwFileVersionLS);
   int Build = LOWORD(lpvsinfo->dwFileVersionLS);

   version = IntToStr(MajorVersion) + "." + IntToStr(MinorVersion)
       + "." + IntToStr(Release) + "." + IntToStr(Build);

   delete [] buffer;

   return version;
   }

