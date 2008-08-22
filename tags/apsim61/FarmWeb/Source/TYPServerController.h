//---------------------------------------------------------------------------

#ifndef TYPServerControllerH
#define TYPServerControllerH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <IWServerControllerBase.hpp>
#include <IWApplication.hpp>
#include <IWInit.hpp>
//---------------------------------------------------------------------------
class TServerController  : public TIWServerControllerBase
   {
   __published:
      void __fastcall OnCreateNewSession(
          TIWApplication *ASession, TIWBaseForm *&VMainForm);

   private:


   public:		
      __fastcall TServerController(TComponent* Owner);
   };

//---------------------------------------------------------------------------
extern PACKAGE TServerController *ServerController;
//---------------------------------------------------------------------------
#endif
 