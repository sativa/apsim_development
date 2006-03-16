//---------------------------------------------------------------------------

#ifndef ApsimNCH
#define ApsimNCH
//---------------------------------------------------------------------------
#include "netcdfcpp.h"
#include "TCSV.h"

#include <vcl.h>
//---------------------------------------------------------------------------
//*Note - This wrapper class requires that the C++ NetCDF dll is include in the
//        project
class __declspec(dllexport) ApsimNC
   {
   private:
      void __fastcall FreeMem(void);
      void __fastcall ParseTitle(String Title, TStringList *Facs, TStringList *Levs);
      void __fastcall InitArray(float *Arr, int Count, float Num);
      void __fastcall InitArray(int *Arr, int Count, int Num);
      void __fastcall SetAttributes(TStringList *_Factors, TStringList **_Levels,
            TStringList *_Traits);

      String __fastcall GetTitle(TStringList *File, String FileName);
      String __fastcall GetTraits(TStringList *File, String FileName,
            int &Index);
      String __fastcall CreateListString(TStringList *List);

      int __fastcall GetMaxVal(int * Arr, int Size);
      int __fastcall FindStringInTSL(TStringList *TSL, String Var);

      bool __fastcall GetFileAttributes();
      bool __fastcall ExtractApsData(TStringList *File, String FileName, float **Data,
            int YearPos, String YearTrait);
      bool __fastcall GetFactorCoords(TStringList *File, String FileName, long *Coords,
            long *Size);
      bool __fastcall GetFactorCoords(TStringList *Levs, long *Coords, long *Size);
      bool __fastcall Create(void);


      
      //Globals
      NcFile *nc;                   //NetCDF object
      String CurrentNCFile;         //String holding the filename of the NetCDF
                                    //that the following TSL belong to.
      TStringList *Factors;         //TSL of Factors for the NetCDF
      TStringList **Levels;         //An array of TSL's of Levels for the NetCDF
      TCSV *Traits;          //TSL of Traits for the NetCDF

   public:
      __fastcall ApsimNC(TStringList *_Factors, TStringList **_Levels,
            TStringList *_Traits, String NCFileName);
      __fastcall ApsimNC(String NCFileName, bool ReadOnly = false);
      __fastcall ~ApsimNC(void);
      void __fastcall GetFileAttributes(TStringList *_Factors, TStringList **_Levels,
            TStringList *_Traits);

      void GetTraits(TStringList *_Traits);
      void SetTraits(TStringList *_Traits);
      int __fastcall NumFactors(void);
      int __fastcall NumYears(void);
      int __fastcall NumSites(void);
      int __fastcall GetStartYear(void);

      String __fastcall CurrentFile(void);

      bool __fastcall CheckFirstDone(TStringList *Facs);
      bool __fastcall CheckAllDone(TStringList *Facs);
      bool __fastcall GetSiteData(TStringList *Facs, String Trait, float *Data);
      bool __fastcall GetAllData(TStringList * Facs, String Trait, float ** Data);
      bool __fastcall GetSiteList(TStringList *Sites);
      bool __fastcall AddFile(String ApsFileName, String YearTrait);
      bool __fastcall CheckScenario(TStringList *CheckFacs, TStringList *CheckLevs);
      bool __fastcall Synchronise(void);
      bool __fastcall Refresh(void);
      bool locked;

   };
#endif
