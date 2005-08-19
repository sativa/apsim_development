//---------------------------------------------------------------------------

#ifndef TRainfallFormH
#define TRainfallFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "Planner.hpp"
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include <ComCtrls.hpp>
#include <Grids.hpp>
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
#include "AdvFileNameEdit.hpp"
#include <Buttons.hpp>
#include <Menus.hpp>
#include <ImgList.hpp>
#include <ToolWin.hpp>
#include <ExtCtrls.hpp>
#include <Dialogs.hpp>
#include <boost\date_time\gregorian\gregorian.hpp>
//---------------------------------------------------------------------------
class TRainfallForm : public TForm
   {
   __published:	// IDE-managed Components
      TAdvStringGrid *grid;
      TImageList *ImageList1;
      TToolBar *ToolBar1;
      TToolButton *NewButton;
      TToolButton *OpenButton;
      TToolButton *SaveButton;
      TOpenDialog *OpenDialog;
      TToolButton *ExitButton;
      TPanel *Panel2;
      TSaveDialog *SaveDialog;
      TUpDown *YearUpDown;
      TEdit *YearEdit;
   TPanel *TotalRainLabel;
      void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
      void __fastcall SaveButtonClick(TObject *Sender);
      void __fastcall OpenButtonClick(TObject *Sender);
      void __fastcall NewButtonClick(TObject *Sender);
      void __fastcall ExitButtonClick(TObject *Sender);
      void __fastcall gridCellsChanged(TObject *Sender, TRect &R);
      void __fastcall gridIsFixedCell(TObject *Sender, int ARow, int ACol,
          bool &IsFixed);
      void __fastcall YearUpDownChangingEx(TObject *Sender, bool &AllowChange,
          short NewValue, TUpDownDirection Direction);
   private:
      bool dirty;
      std::string fileName;
      bool clearingCells;
      typedef std::map<boost::gregorian::date, float> Rainfall;
      Rainfall rainfall;
      int gridYear;

      //---------------------------------------------------------------------------
      // Setup the month columns in grid based on date.
      //---------------------------------------------------------------------------
      void setupMonthsInGrid(void);

      //---------------------------------------------------------------------------
      // Clear the grid back to default values.
      //---------------------------------------------------------------------------
      void clear(void);

      //---------------------------------------------------------------------------
      // open a met file
      //---------------------------------------------------------------------------
      void open(const std::string& fileName);

      //---------------------------------------------------------------------------
      // save all data in grid.
      //---------------------------------------------------------------------------
      void save(void);

      //---------------------------------------------------------------------------
      // Save the data if it is dirty.
      //---------------------------------------------------------------------------
      void saveIfNecessary(void);

      //---------------------------------------------------------------------------
      // populate rainfall grid for the specified year.
      //---------------------------------------------------------------------------
      void populateGrid(int year);

      //---------------------------------------------------------------------------
      // save rainfall grid.
      //---------------------------------------------------------------------------
      void saveGrid(void);

      //---------------------------------------------------------------------------
      // return true if cell is fixed.
      //---------------------------------------------------------------------------
      bool isCellFixed(int ARow, int ACol);
   public:		// User declarations
      __fastcall TRainfallForm(TComponent* Owner);

      void setup(const std::string& fileName);
      std::string getFileName(void) {return fileName;}
   };
//---------------------------------------------------------------------------
extern PACKAGE TRainfallForm *RainfallForm;
//---------------------------------------------------------------------------
#endif
