//---------------------------------------------------------------------------

#ifndef TSEGShapeH
#define TSEGShapeH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <Qrctrls.hpp>
#include <QuickRpt.hpp>
#include <vector>
#include <string>
#include "TSEGTable.h"
#include "ReportMacros.h"
//---------------------------------------------------------------------------
// This class encapsulates a regular TQRShape - just providing a
// different name instead of TQRShape.
//---------------------------------------------------------------------------
class PACKAGE TShape : public TQRShape
   {
   private:
   protected:
   public:
      __fastcall TShape(TComponent* Owner);
   __published:
   };
//---------------------------------------------------------------------------
#endif
