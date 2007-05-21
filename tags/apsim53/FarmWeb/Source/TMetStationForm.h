//---------------------------------------------------------------------------
#ifndef TMetStationFormH
#define TMetStationFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWCompLabel.hpp"
#include "IWControl.hpp"
#include "IWVCLBaseControl.hpp"
#include "IWOutlookBar.hpp"
#include "IWCompEdit.hpp"
#include "IWCompListbox.hpp"
#include "IWTMSCal.hpp"
#include "IWCompButton.hpp"
#include <ImgList.hpp>
#include "IWCompRectangle.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompMemo.hpp"
#include "Data.h"
#include "IWExtCtrls.hpp"
#include "IWHTMLControls.hpp"

class TWebSession;
//---------------------------------------------------------------------------
// This form shows the user all paddock information.
//---------------------------------------------------------------------------
class TMetStationForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWLabel *PromptLabel;
      TIWComboBox *RegionCombo;
      TIWRectangle *IWRectangle1;
      TIWListbox *MetStationList;
      TIWLabel *IWLabel1;
      TIWFile *ImportFile;
   TIWImageFile *IWImageFile2;
   TIWLink *AddRegionButton;
   TIWImageFile *IWImageFile3;
   TIWLink *DeleteRegionButton;
   TIWLink *DeleteMetStationButton;
   TIWImageFile *IWImageFile1;
   TIWLink *ImportButton;
   TIWImageFile *IWImageFile4;
      void __fastcall DeleteRegionButtonClick(TObject *Sender);
      void __fastcall RegionComboChange(TObject *Sender);
      void __fastcall AddRegionButtonClick(TObject *Sender);
      void __fastcall DeleteMetStationButtonClick(TObject *Sender);
      void __fastcall ImportButtonClick(TObject *Sender);
   private:
      TWebSession* webSession;
      Data* data;

      //---------------------------------------------------------------------------
      // populate the region combo
      //---------------------------------------------------------------------------
      void populateRegionCombo();

      //---------------------------------------------------------------------------
      // Populate the MetStationList list.
      //---------------------------------------------------------------------------
      void populateMetStationList();

      //---------------------------------------------------------------------------
      // User has finished entering a new region name
      //---------------------------------------------------------------------------
      void __fastcall addRegionCallback(bool okClicked,
                                        AnsiString text1,
                                        AnsiString text2,
                                        AnsiString text3,
                                        AnsiString text4);

      //---------------------------------------------------------------------------
      // Callback for deleteRegion
      //---------------------------------------------------------------------------
      void __fastcall deleteRegionCallback(bool yesClicked);

      //---------------------------------------------------------------------------
      // Callback for delete metstation
      //---------------------------------------------------------------------------
      void __fastcall deleteMetStationCallback(bool yesClicked);


   public:
      __fastcall TMetStationForm(TComponent* Owner);

      void setup(TWebSession* webSession, Data* d);

   };
//---------------------------------------------------------------------------
#endif
