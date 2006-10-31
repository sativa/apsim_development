//---------------------------------------------------------------------------


#pragma hdrstop

#include "TCSV.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

void __fastcall TCSV::ParseCSV(System::AnsiString Value)
   {
   // function to parse a line using only commas as delimiters because
   // TStringList->CommaText parses with spaces and commas
   // this could be extended to handle enclosed , etc

   // remove " and parse for ,
   //Value = StringReplace(Value,"\"","",TReplaceFlags() << rfReplaceAll);
   this->Clear();
   int p0=0,p1;
   while((p1 = Value.SubString(p0+1,Value.Length()).Pos(',')))
      {
      this->Add(Value.SubString(p0+1,p1-1).Trim());
      p0 += p1;
      }
   this->Add(Value.SubString(p0+1,Value.Length()).Trim());

   }
//---------------------------------------------------------------------------

System::AnsiString __fastcall TCSV::GetCSV(void)
   {
   String Txt;
   for(int i=0;i < this->Count;i++)
      {
      if(i)
         {
         Txt += ",";
         }
      Txt += this->Strings[i];
      }
   return Txt;
   }