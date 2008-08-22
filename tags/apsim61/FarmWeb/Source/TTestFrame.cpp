//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "TTestFrame.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompLabel"
#pragma link "IWContainer"
#pragma link "IWControl"
#pragma link "IWHTMLContainer"
#pragma link "IWVCLBaseContainer"
#pragma link "IWVCLBaseControl"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TTestFrame::TTestFrame(TComponent* Owner)
        : TFrame(Owner)
{

}
//---------------------------------------------------------------------------

