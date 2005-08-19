//---------------------------------------------------------------------------

#ifndef TDataH
#define TDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ADODB.hpp>
#include <DB.hpp>
//---------------------------------------------------------------------------
class TData : public TDataModule
   {
   __published:	// IDE-managed Components
      TADOConnection *connection;
      TADOTable *region;
      TADOTable *site;
      TADOTable *soil;
      TADOTable *soilData;
      TADOTable *crop;
      TADOTable *cropData;
      TADOTable *soilLayeredData;
      TADOTable *soilType;
      TADOTable *predictedLLConstants;
   private:	// User declarations
   public:		// User declarations
      __fastcall TData(TComponent* Owner);
   };
//---------------------------------------------------------------------------
extern PACKAGE TData *Data;
//---------------------------------------------------------------------------
#endif
