//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "THGrid.h"
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(THGrid *)
{
   new THGrid(NULL);
}
//---------------------------------------------------------------------------
__fastcall THGrid::THGrid(TComponent* Owner)
   : THyperGrid(Owner)
   {
   }
//---------------------------------------------------------------------------
namespace Thgrid
{
   void __fastcall PACKAGE Register()
   {                                                    
      TComponentClass classes[1] = {__classid(THGrid)};
      RegisterComponents("APSRU", classes, 0);
   }
}
//---------------------------------------------------------------------------
 