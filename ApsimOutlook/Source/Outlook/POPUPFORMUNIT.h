
/****************************************************************************\

    TPopupForm.h -- Created by Damon Chandler <dmc27@cornell.edu>

\****************************************************************************/

//---------------------------------------------------------------------------
#ifndef PopupFormUnitH
#define PopupFormUnitH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <Buttons.hpp>
//---------------------------------------------------------------------------

class TPopupForm : public TForm
{
__published:
   void __fastcall FormMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y);

protected:
   virtual void __fastcall CreateWnd();
   virtual void __fastcall CreateParams(TCreateParams& AParams);
   DYNAMIC void __fastcall VisibleChanging();
   
private:
   MESSAGE void __fastcall CMMouseEnter(TMessage& Msg);
   MESSAGE void __fastcall CMMouseLeave(TMessage& Msg);
   MESSAGE void __fastcall WMActivateApp(TMessage& AMsg);

public:
   __fastcall TPopupForm(TComponent* Owner);

BEGIN_MESSAGE_MAP
   MESSAGE_HANDLER(CM_MOUSEENTER, TMessage, CMMouseEnter)
   MESSAGE_HANDLER(CM_MOUSELEAVE, TMessage, CMMouseLeave)
   MESSAGE_HANDLER(WM_ACTIVATEAPP, TMessage, WMActivateApp)   
END_MESSAGE_MAP(TForm)
};

//---------------------------------------------------------------------------
extern PACKAGE TPopupForm *PopupForm;
//---------------------------------------------------------------------------
#endif
