//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSummary_form.h"
#include "TSummary_analysis.h"
#include <ApsimShared\ApsimDirectories.h>
#include <ApsimShared\ApsimSettings.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TAnalysis_form"
#pragma link "AdvCombo"
#pragma link "ImagePicker"
#pragma resource "*.dfm"
TSummary_form *Summary_form;
//---------------------------------------------------------------------------
__fastcall TSummary_form::TSummary_form(TComponent* Owner)
   : TAnalysis_form(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TSummary_form::FormShow(TObject *Sender)
   {
   TAnalysis_form::FormShow(Sender);

   // optionally remove sheets from form based on ini settings.
   ApsimSettings settings;
   string st;
   settings.read("Outlook Skin|DisabledSummaryChartTabs", st);
   if (st.find("box") != string::npos)
      BoxSheet->TabVisible = false;
   if (st.find("column") != string::npos)
      ColumnSheet->TabVisible = false;
   if (st.find("percentile") != string::npos)
      PercentileSheet->TabVisible = false;
   if (st.find("general") != string::npos)
      GeneralSheet->TabVisible = false;

   TSummary_analysis* summaryAnalysis = dynamic_cast<TSummary_analysis*> (Analysis_ptr);
   if (summaryAnalysis != NULL)
      {
      if (summaryAnalysis->SeriesType == box || summaryAnalysis->SeriesType == box_no_whiskers)
         GalleryPageControl->ActivePageIndex = 0;
      else if (summaryAnalysis->SeriesType == box_column)
         GalleryPageControl->ActivePageIndex = 1;
      else if (summaryAnalysis->SeriesType == bar_percentile_line)
         GalleryPageControl->ActivePageIndex = 2;
      else
         GalleryPageControl->ActivePageIndex = 3;

      BoxWhiskers->Checked = (summaryAnalysis->SeriesType == box);
      setupPercentilePage(BoxEdit1, BoxEdit2, BoxEdit3, BoxEdit4,
                          BoxMedian, BoxMean);
      setupPercentilePage(ColumnEdit1, ColumnEdit2, ColumnEdit3, ColumnEdit4,
                          ColumnMedian, ColumnMean);

      // setup percentile sheet.
      if (summaryAnalysis->Percentiles->Count == 2 || summaryAnalysis->Percentiles->Count == 3)
         {
         BarEdit1->Text = summaryAnalysis->Percentiles->Strings[0];
         if (summaryAnalysis->Percentiles->Strings[1] == "50")
            {
            BarMedian->Checked = true;
            BarEdit2->Text = summaryAnalysis->Percentiles->Strings[2];
            }
         else
            BarEdit2->Text = summaryAnalysis->Percentiles->Strings[1];
         }
      else
         BarMedian->Checked = true;

      // setup general sheet.
      for (int i = 0; i <= 20; i++)
         {
         TListItem* item = PercentileList->Items->Item[i];
         AnsiString st = item->Caption;
         st.Delete(st.Length(), 1);
         if (summaryAnalysis->Percentiles->IndexOf(st) >= 0)
            item->Checked = true;
         }
      PercentileList->Items->Item[21]->Checked = summaryAnalysis->Mean;
      SeriesTypeList->ItemIndex = (int) summaryAnalysis->SeriesType;
      }
   }
//---------------------------------------------------------------------------
void TSummary_form::setupPercentilePage(TEdit* BoxEdit1, TEdit* BoxEdit2,
                                        TEdit* BoxEdit3, TEdit* BoxEdit4,
                                        TCheckBox* BoxMedian, TCheckBox* BoxMean)
   {
   TSummary_analysis* summaryAnalysis = dynamic_cast<TSummary_analysis*> (Analysis_ptr);
   if (summaryAnalysis->Percentiles->Count < 4)
      {
      BoxEdit1->Text = "10";
      BoxEdit2->Text = "25";
      BoxEdit3->Text = "75";
      BoxEdit4->Text = "90";
      BoxMedian->Checked = true;
      }
   else
      {
      BoxEdit1->Text = summaryAnalysis->Percentiles->Strings[0];
      BoxEdit2->Text = summaryAnalysis->Percentiles->Strings[1];
      if (summaryAnalysis->Percentiles->Strings[2] == "50")
         {
         BoxMedian->Checked = true;
         BoxEdit3->Text = summaryAnalysis->Percentiles->Strings[3];
         BoxEdit4->Text = summaryAnalysis->Percentiles->Strings[4];
         }
      else
         {
         BoxEdit3->Text = summaryAnalysis->Percentiles->Strings[2];
         BoxEdit4->Text = summaryAnalysis->Percentiles->Strings[3];
         }
      }
   BoxMean->Checked = summaryAnalysis->Mean;
   }
//---------------------------------------------------------------------------
void __fastcall TSummary_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   TAnalysis_form::FormClose(Sender, Action);
   if (ModalResult == mrOk)
      {
      TSummary_analysis* summaryAnalysis = dynamic_cast<TSummary_analysis*> (Analysis_ptr);
      if (summaryAnalysis != NULL)
         {
         summaryAnalysis->Percentiles->Clear();
         if (GalleryPageControl->ActivePageIndex == 0)
            {
            // box plots
            summaryAnalysis->Percentiles->Add(StrToInt(BoxEdit1->Text));
            summaryAnalysis->Percentiles->Add(StrToInt(BoxEdit2->Text));
            if (BoxMedian->Checked)
               summaryAnalysis->Percentiles->Add(50);
            summaryAnalysis->Percentiles->Add(StrToInt(BoxEdit3->Text));
            summaryAnalysis->Percentiles->Add(StrToInt(BoxEdit4->Text));
            summaryAnalysis->Mean = BoxMean->Checked;
            summaryAnalysis->Minimum = false;
            summaryAnalysis->Maximum = false;
            summaryAnalysis->Count = false;
            summaryAnalysis->TableOnly = false;
            if (BoxWhiskers->Checked)
               summaryAnalysis->SeriesType = box;
            else
               summaryAnalysis->SeriesType = box_no_whiskers;
            }
         else if (GalleryPageControl->ActivePageIndex == 1)
            {
            // column plots
            summaryAnalysis->Percentiles->Add(StrToInt(ColumnEdit1->Text));
            summaryAnalysis->Percentiles->Add(StrToInt(ColumnEdit2->Text));
            if (ColumnMedian->Checked)
               summaryAnalysis->Percentiles->Add(50);
            summaryAnalysis->Percentiles->Add(StrToInt(ColumnEdit3->Text));
            summaryAnalysis->Percentiles->Add(StrToInt(ColumnEdit4->Text));
            summaryAnalysis->Mean = ColumnMean->Checked;
            summaryAnalysis->Minimum = false;
            summaryAnalysis->Maximum = false;
            summaryAnalysis->Count = false;
            summaryAnalysis->TableOnly = false;
            summaryAnalysis->SeriesType = box_column;
            }
         else if (GalleryPageControl->ActivePageIndex == 2)
            {
            // percentile bar plots
            summaryAnalysis->Percentiles->Add(StrToInt(BarEdit1->Text));
            if (BarMedian->Checked)
               summaryAnalysis->Percentiles->Add(50);
            summaryAnalysis->Percentiles->Add(StrToInt(BarEdit2->Text));
            summaryAnalysis->Mean = true;
            summaryAnalysis->Minimum = false;
            summaryAnalysis->Maximum = false;
            summaryAnalysis->Count = false;
            summaryAnalysis->TableOnly = false;
            summaryAnalysis->SeriesType = bar_percentile_line;
            }
         else
            {
            // general
            for (int i = 0; i <= 20; i++)
               {
               TListItem* item = PercentileList->Items->Item[i];
               if (item->Checked)
                  {
                  AnsiString st = item->Caption;
                  st.Delete(st.Length(), 1);
                  summaryAnalysis->Percentiles->Add(StrToInt(st));
                  }
               }
            summaryAnalysis->Mean = PercentileList->Items->Item[21]->Checked;
            summaryAnalysis->Minimum = false;
            summaryAnalysis->Maximum = false;
            summaryAnalysis->Count = false;
            summaryAnalysis->TableOnly = false;
            summaryAnalysis->SeriesType = (SeriesTypes)SeriesTypeList->ItemIndex;
            }
         }
      }
   }

