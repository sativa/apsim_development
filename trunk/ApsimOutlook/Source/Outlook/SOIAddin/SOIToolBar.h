//---------------------------------------------------------------------------

#ifndef SOIToolBarH
#define SOIToolBarH

#include <vector>
#include <set>
#include "ToolBarAddIn.h"
#include <apsimShared\ApsimSettings.h>

class SOIToolBar : public ToolBarAddInBase
{
   public:
      //const
      SOIToolBar(const std::string& parameters);

      ~SOIToolBar();

      // return a list of TToolButton*s
      virtual void decorateToolBar(TToolBar* toolbar);

      // given the source data object, perform all calculations and store all new data
      // in the returned TAPSTable. Can have the side effect of modifying the passed
      // in table, as well as returning a new copy.
      virtual void doCalculations(TAPSTable& data);

      // returns true if this addin needs to run its calculations again
      virtual bool needsUpdate();

      // indicates that this addin needs to run its calculations again
      virtual void youNeedUpdating();

      int FPhase_month;
      bool SOI_enabled;
      vector<unsigned> phasesToInclude;
      bool allYears;

   private:
      ApsimSettings settings;
      bool needs_update;
      void __fastcall buttonClick(TObject* Sender);

      static TToolButton* SOI_button;
      static Graphics::TBitmap* glyph;
      static int glyph_position;
      static TToolBar* Toolbar;

      string sowYearFieldName;
      AnsiString FSOI_data_file;
      AnsiString FYear_field_name;
      int FFontHeight;
      std::vector< std::string > FPhase_names;

      void load();
      void save();

      class soi
      {
         public:
            int Year;
            int Month;
            int Phase;

            soi(void)
               {}
            soi(int year, int month)
               {
               Year = year;
               Month = month;
               }
            bool operator== (const soi& rhs) const
               {return (Year == rhs.Year && Month == rhs.Month);}
            bool operator!= (const soi& rhs) const
               {return !(*this == rhs);}
            bool operator< (const soi& rhs) const
               {return (Year < rhs.Year ||
                        (Year == rhs.Year && Month < rhs.Month));
               }
            friend std::istream& operator>>(std::istream& in, soi& to)
               {
               in >> to.Year >> to.Month >> to.Phase;
               return in;
               }
         };

      typedef std::set <soi, std::less<soi> > soi_set;
      soi_set soi_phases;

      void Read_all_soi_data (void);
      void Get_phase (int Year, int Month, unsigned& SOI_phase, std::string& SOI_phase_st);
      void calcSowYearFieldName(const TAPSRecord& record) throw(std::runtime_error);
      unsigned getSowYear(const TAPSRecord& record) throw(std::runtime_error);

/*   __published:
      __property int Phase_month = {read=FPhase_month, write=FPhase_month};
      __property AnsiString Year_field_name = {read=FYear_field_name, write=FYear_field_name};
      __property AnsiString SOI_data_file = {read=FSOI_data_file, write=FSOI_data_file};
      __property int FontHeight = {read=FFontHeight, write=FFontHeight};
*/

};



//---------------------------------------------------------------------------
#endif
