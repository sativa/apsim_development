//----------------------------------------------------------------------------
#ifndef aboutH
#define aboutH
//----------------------------------------------------------------------------
#include <ExtCtrls.hpp>
#include <Buttons.hpp>
#include <StdCtrls.hpp>
#include <Controls.hpp>
#include <Forms.hpp>
#include <Graphics.hpp>
#include <Classes.hpp>
#include <Windows.hpp>
#include <System.hpp>
#include "verslab.hpp"
#include "verslab.hpp"     
//----------------------------------------------------------------------------
class TAboutBox : public TForm
{
__published:
   TBitBtn *BitBtn1;
   TImage *Image1;
   TLabel *VersionLabel;
private:
public:
	virtual __fastcall TAboutBox(TComponent *Owner);
};
//----------------------------------------------------------------------------
extern TAboutBox *AboutBox;
//----------------------------------------------------------------------------
#endif	
