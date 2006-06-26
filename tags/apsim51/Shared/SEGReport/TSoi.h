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
      bool negativePhase;
      bool positivePhase;
      bool fallingPhase;
      bool risingPhase;
      bool zeroPhase;
      bool getSOIFromSource;

      AnsiString __fastcall getMonth(void);
      void __fastcall setMonth(AnsiString month);
      void __fastcall setFilename(AnsiString filename);
      void __fastcall setNegativePhase(bool negative);
      void __fastcall setPositivePhase(bool positive);
      void __fastcall setFallingPhase(bool falling);
      void __fastcall setRisingPhase(bool rising);
      void __fastcall setZeroPhase(bool zero);
      void __fastcall setGetSoiFromSource(bool getFromSource);

      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);
      std::string getSowYearFieldName(TSEGTable* data) const throw (std::runtime_error);
      void readSoiData(void) throw(std::runtime_error);
      void getPhase(unsigned Year, unsigned Month,
                    unsigned& SOI_phase, std::string& SOI_phase_st) throw (std::runtime_error);
      bool keepPhase(unsigned phase);

   public:
      __fastcall TSOI(TComponent* owner);
      __fastcall ~TSOI();

      virtual void setProperty(const std::string& propertyName,
                               const std::string& propertyValue);
      
   __published:
      __property AnsiString month = {read=getMonth, write=setMonth};
      __property AnsiString filename = {read=soiFilename, write=setFilename};
      __property bool negative = {read=negativePhase, write=setNegativePhase};
      __property bool positive = {read=positivePhase, write=setPositivePhase};
      __property bool falling = {read=fallingPhase, write=setFallingPhase};
      __property bool rising = {read=risingPhase, write=setRisingPhase};
      __property bool zero = {read=zeroPhase, write=setZeroPhase};
      __property bool GetSOIFromSource = {read=getSOIFromSource, write=setGetSoiFromSource};
   };
#endif
