//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompButton.hpp"
#include "IWControl.hpp"
#include "IWVCLBaseControl.hpp"
#include <ADODB.hpp>

//---------------------------------------------------------------------------
class TIWForm1: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWButton *IWButton1;
   void __fastcall IWButton1Click(TObject *Sender);

   private:	// User declarations
      Adodb::TADOConnection* connection;
      void open(const std::string& fileName);
      vector<double> getLayered(XMLDocument* doc, const char* layeredType);
      vector<unsigned> getLayeredInt(XMLDocument* doc, const char* layeredType);

      void convertSoils();
      void copyReportsFromOnePaddockToAnother();
      void changeReportDates();


   public:		// User declarations
           __fastcall TIWForm1(TComponent* Owner);
           __fastcall ~TIWForm1();
   };
//---------------------------------------------------------------------------
#endif
