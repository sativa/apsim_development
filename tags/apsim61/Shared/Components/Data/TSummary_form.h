//---------------------------------------------------------------------------
#ifndef TSummary_formH
#define TSummary_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TAnalysis_form.h"
#include <Buttons.hpp>
#include <Grids.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include "TAnalysis_series.h"
#include "AdvCombo.hpp"
#include "ImagePicker.hpp"
#include <general\inifile.h>
//---------------------------------------------------------------------------
class TSummary_form : public TAnalysis_form
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TImageList *GalleryImageList;
   TPageControl *GalleryPageControl;
   TTabSheet *BoxSheet;
   TTabSheet *ColumnSheet;
   TTabSheet *PercentileSheet;
   TTabSheet *GeneralSheet;
   TLabel *Label4;
   TImage *Image1;
   TEdit *BoxEdit4;
   TEdit *BoxEdit3;
   TEdit *BoxEdit2;
   TEdit *BoxEdit1;
   TLabel *Label5;
   TLabel *Label6;
   TLabel *Label7;
   TLabel *Label8;
   TCheckBox *BoxMedian;
   TCheckBox *BoxMean;
   TCheckBox *BoxWhiskers;
   TLabel *Label9;
   TImage *Image2;
   TEdit *ColumnEdit4;
   TEdit *ColumnEdit3;
   TLabel *Label10;
   TLabel *Label11;
   TEdit *ColumnEdit2;
   TEdit *ColumnEdit1;
   TLabel *Label12;
   TLabel *Label13;
   TCheckBox *ColumnMedian;
   TCheckBox *ColumnMean;
   TLabel *Label14;
   TImage *Image3;
   TEdit *BarEdit2;
   TEdit *BarEdit1;
   TLabel *Label15;
   TLabel *Label16;
   TCheckBox *BarMedian;
   TListView *PercentileList;
   TLabel *Label18;
   TImagePicker *SeriesTypeList;
   TLabel *Label17;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
   void TSummary_form::setupPercentilePage(TEdit* BoxEdit1, TEdit* BoxEdit2,
                                           TEdit* BoxEdit3, TEdit* BoxEdit4,
                                           TCheckBox* BoxMedian, TCheckBox* BoxMean);
public:		// User declarations
   __fastcall TSummary_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSummary_form *Summary_form;
//---------------------------------------------------------------------------
#endif
