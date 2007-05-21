//---------------------------------------------------------------------------

#ifndef TTestFrameH
#define TTestFrameH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWColor.hpp>
#include <IWRegion.hpp>
#include "IWBaseControl.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompLabel.hpp"
#include "IWContainer.hpp"
#include "IWControl.hpp"
#include "IWHTMLContainer.hpp"
#include "IWVCLBaseContainer.hpp"
#include "IWVCLBaseControl.hpp"
//---------------------------------------------------------------------------
class TTestFrame: public TFrame
{
__published:	// IDE-managed Components
 TIWRegion *IWFrameRegion;
   TIWLabel *IWLabel1;
private:	// User declarations
public:		// User declarations
        __fastcall TTestFrame(TComponent* Owner);
};
//---------------------------------------------------------------------------
#endif
