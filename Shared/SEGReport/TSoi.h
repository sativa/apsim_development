//---------------------------------------------------------------------------

#ifndef TSOIH
#define TSOIH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this adds an SOI column
//---------------------------------------------------------------------------
class TSOI : public TSEGTable
   {
   private:
      AnsiString soiFilename;
      unsigned soiMonth;
      std::vector<std::string> phaseNames;
      typedef std::map<std::string, unsigned, std::less<std::string> > Phases;
      Phases phases;

      AnsiString __fastcall getMonth(void);
      void __fastcall setMonth(AnsiString month);
      void __fastcall setFilename(AnsiString filename);
      virtual void createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);
      std::string getSowYearFieldName(TSEGTable* data) const throw (std::runtime_error);
      void readSoiData(void) throw(std::runtime_error);
      void getPhase(unsigned Year, unsigned Month,
                    unsigned& SOI_phase, std::string& SOI_phase_st) throw (std::runtime_error);

   public:
      __fastcall TSOI(TComponent* owner);
      __fastcall ~TSOI();

   __published:
      __property AnsiString month = {read=getMonth, write=setMonth};
      __property AnsiString filename = {read=soiFilename, write=setFilename};
   };
#endif
