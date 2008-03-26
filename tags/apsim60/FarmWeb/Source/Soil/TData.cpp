//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TData *Data;
//---------------------------------------------------------------------------
__fastcall TData::TData(TComponent* Owner)
   : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
