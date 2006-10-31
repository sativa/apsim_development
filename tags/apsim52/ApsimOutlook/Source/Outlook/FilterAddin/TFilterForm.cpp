//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TFilterForm.h"
#include "FiltAddIn.h"
#include <general\vcl_functions.h>
#include <general\stl_functions.h>
#include <general\path.h>
#include <ApsimShared\ApsimDirectories.h>
using namespace std;

static const unsigned int MAX_NUM_MRU_FILTERS = 20;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "kbmMemTable"
#pragma link "flt_box"
#pragma resource "*.dfm"
TFilterForm *FilterForm;
//---------------------------------------------------------------------------
__fastcall TFilterForm::TFilterForm(TComponent* Owner)
   : TForm(Owner)
   {
   inFormShow = false;
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FormShow(TObject *Sender)
   {
   inFormShow = true;
   // populate the comboboxes with most recent filter values.
   vector<string> mostRecentFilters;
   settings.read("Outlook Filters|filter", mostRecentFilters);
   Stl_2_tstrings(mostRecentFilters, FilterCombo1->Items);
   Stl_2_tstrings(mostRecentFilters, FilterCombo2->Items);
   Stl_2_tstrings(mostRecentFilters, FilterCombo3->Items);
   Stl_2_tstrings(mostRecentFilters, FilterCombo4->Items);
   Stl_2_tstrings(mostRecentFilters, FilterCombo5->Items);

   // clear the text property of all filter combos
   FilterCombo1->Text = "";
   FilterCombo2->Text = "";
   FilterCombo3->Text = "";
   FilterCombo4->Text = "";
   FilterCombo5->Text = "";
   FilterCombo1->Tag = -1;
   FilterCombo2->Tag = -1;
   FilterCombo3->Tag = -1;
   FilterCombo4->Tag = -1;
   FilterCombo5->Tag = -1;

   // put the Filter AddIn's current filter values into the combo boxes.
   vector<string> filters = filterAddIn->getFilters();
   if (filters.size() >= 1)
      {
      FilterCombo1->ItemIndex = FilterCombo1->Items->IndexOf(filters[0].c_str());
      FilterCombo1Change(NULL);
      }
   if (filters.size() >= 2)
      {
      FilterCombo2->ItemIndex = FilterCombo2->Items->IndexOf(filters[1].c_str());
      FilterCombo2Change(NULL);
      }
   if (filters.size() >= 3)
      {
      FilterCombo3->ItemIndex = FilterCombo3->Items->IndexOf(filters[2].c_str());
      FilterCombo3Change(NULL);
      }
   if (filters.size() >= 4)
      {
      FilterCombo4->ItemIndex = FilterCombo4->Items->IndexOf(filters[3].c_str());
      FilterCombo4Change(NULL);
      }
   if (filters.size() >= 5)
      {
      FilterCombo5->ItemIndex = FilterCombo5->Items->IndexOf(filters[4].c_str());
      FilterCombo5Change(NULL);
      }
   inFormShow = false;
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      // get a list of unique filters from the combo boxes.
      vector<string> filters;
      if (FilterCombo1->Text != "")
         addToVectorIfUnique(filters, FilterCombo1->Text.c_str());
      if (FilterCombo2->Text != "")
         addToVectorIfUnique(filters, FilterCombo2->Text.c_str());
      if (FilterCombo3->Text != "")
         addToVectorIfUnique(filters, FilterCombo3->Text.c_str());
      if (FilterCombo4->Text != "")
         addToVectorIfUnique(filters, FilterCombo4->Text.c_str());
      if (FilterCombo5->Text != "")
         addToVectorIfUnique(filters, FilterCombo5->Text.c_str());

      // give current selected filters back to filter add-in
      filterAddIn->setFilters(filters);

      // make up a most recent filter list that we're going to write back
      // to the .ini file.
      vector<string> strings;
      settings.read("Outlook Filters|filter", strings);
      list<string> mostRecentFilters;
      copy(strings.begin(), strings.end(), back_inserter(mostRecentFilters));
      for (vector<string>::iterator i = filters.begin();
                                    i != filters.end();
                                    i++)
         {
         // if the filter is already in the list then move it to the front
         // of the list.
         list<string>::iterator mrfI = find(mostRecentFilters.begin(),
                                            mostRecentFilters.end(), *i);
         if (mrfI != mostRecentFilters.end())
            mostRecentFilters.erase(mrfI);
         mostRecentFilters.push_front(*i);
         }
      while (mostRecentFilters.size() > MAX_NUM_MRU_FILTERS)
         mostRecentFilters.erase(--mostRecentFilters.end());

      strings.erase(strings.begin(), strings.end());
      copy(mostRecentFilters.begin(), mostRecentFilters.end(), back_inserter(strings));
      settings.write("Outlook Filters|filter", strings);
      }
   }
//---------------------------------------------------------------------------
void TFilterForm::addToVectorIfUnique(vector<string>& strings, const string& st)
   {
   if (find(strings.begin(), strings.end(), st) == strings.end())
      strings.push_back(st);
   }
//---------------------------------------------------------------------------
void TFilterForm::PutFilterInCombo(TComboBox* combo, AnsiString filter)
   {
   if (!inFormShow && filter != "")
      {
      // We need to put the filter string (from the filter box) into
      // the combo box list so that it appears in the edit box of
      // the combo box.

      // Use the tag field of the combo box as an integer denoting the
      // items index where we temporarily put a filter string from the
      // filter box.  If we need to, we can reuse this index for future
      // filter strings.

      // Do we need to add the current filter string or is it already there?
      combo->ItemIndex = combo->Items->IndexOf(filter);
      if (combo->ItemIndex == -1)
         {
         // We do need to add it.  Have we added a previous temporary filter
         // that we can use now.
         if (combo->Tag != -1)
            {
            // yes
            combo->Items->Strings[combo->Tag] = filter;
            }
         else
            {
            // no - create a new temporary string in combo box list.
            combo->Items->Append(filter);
            combo->Tag = combo->Items->Count-1;
            }
         combo->ItemIndex = combo->Tag;
         }
      }
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FilterCombo1Change(TObject *Sender)
   {
   MemTable1->Filter = FilterCombo1->Text;
//   FilterBox1->Items->Text = FilterCombo1->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FilterBox1Change(TObject *Sender)
   {
   PutFilterInCombo(FilterCombo1, FilterBox1->FilterStr);
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FilterCombo2Change(TObject *Sender)
   {
   MemTable2->Filter = FilterCombo2->Text;
//   FilterBox2->FilterStr = FilterCombo2->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FilterBox2Change(TObject *Sender)
   {
   PutFilterInCombo(FilterCombo2, FilterBox2->FilterStr);
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FilterCombo3Change(TObject *Sender)
   {
   MemTable3->Filter = FilterCombo3->Text;
//   FilterBox3->FilterStr = FilterCombo3->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FilterBox3Change(TObject *Sender)
   {
   PutFilterInCombo(FilterCombo3, FilterBox3->FilterStr);
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FilterCombo4Change(TObject *Sender)
   {
   MemTable4->Filter = FilterCombo4->Text;
//   FilterBox4->FilterStr = FilterCombo4->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FilterBox4Change(TObject *Sender)
   {
   PutFilterInCombo(FilterCombo4, FilterBox4->FilterStr);
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FilterCombo5Change(TObject *Sender)
   {
   MemTable5->Filter = FilterCombo5->Text;
//   FilterBox5->FilterStr = FilterCombo5->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FilterBox5Change(TObject *Sender)
   {
   PutFilterInCombo(FilterCombo5, FilterBox5->FilterStr);
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::TurnOffButtonClick(TObject *Sender)
   {
   vector<string> filters;
   filterAddIn->setFilters(filters);
   ModalResult = mrCancel;
   Close();
   }
//---------------------------------------------------------------------------

