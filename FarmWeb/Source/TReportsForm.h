//---------------------------------------------------------------------------

#ifndef TReportsFormH
#define TReportsFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompLabel.hpp"
#include "IWControl.hpp"
#include "IWOutlookBar.hpp"
#include "IWVCLBaseControl.hpp"
#include <ImgList.hpp>
#include "IWCompListbox.hpp"
#include "IWCompButton.hpp"
#include "IWExtCtrls.hpp"
#include "IWCompRectangle.hpp"
#include "IWHTMLControls.hpp"
class Data;
class TWebSession;
//---------------------------------------------------------------------------
class TReportsForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWRectangle *IWRectangle1;
      TIWImageFile *IWImageFile1;
      TIWLink *DeleteButton;
      TIWImageFile *IWImageFile2;
      TIWLink *BackButton;
      TIWListbox *ReportList;
      TIWImageFile *IWImageFile3;
      TIWLink *ShowButton;
      TIWImageFile *HelpImage;
      TIWLink *HelpButton;
      TIWImageFile *IWImageFile4;
      TIWLink *RenameButton;
      void __fastcall DeleteButtonClick(TObject *Sender);
      void __fastcall PaddockButtonClick(TObject *Sender);
      void __fastcall HelpButtonClick(TObject *Sender);
      void __fastcall ShowButtonClick(TObject *Sender);
      void __fastcall RenameButtonClick(TObject *Sender);
   private:	// User declarations
      TWebSession* webSession;
      Data* data;
      std::string userName;
      std::string paddockName;
      bool fromGrowerManagement;
      std::vector<std::string> reportNames;


      void suckInReports(void);
      void __fastcall deleteCallback(bool deleteConfirmed);
      void populateReportList(void);
      void suckInReportsMatching(const std::string& extension);
      void writeReportHtml(const std::string& fileName);

      //---------------------------------------------------------------------------
      // User has finished renaming a report.
      //---------------------------------------------------------------------------
      void __fastcall renameCallback(bool okClicked,
                                     AnsiString text1,
                                     AnsiString text2,
                                     AnsiString text3,
                                     AnsiString text4);


   public:		// User declarations
      __fastcall TReportsForm(TComponent* Owner);

      void setup(TWebSession* webSession,
                 Data* d,
                 const std::string& userN,
                 bool fromGrowerManagement);

   };
//---------------------------------------------------------------------------
#endif
