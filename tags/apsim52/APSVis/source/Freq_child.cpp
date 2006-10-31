//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl\vcl.h>
#pragma hdrstop

#include "Freq_child.h"
#include <chart\high_level\prob_chart.h>
#include <chart\high_level\scatter_format.h>
#include <chart\high_level\bar_format.h>
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
__fastcall Freq_child::Freq_child(TComponent *Owner)
  	: TChart_child(Owner)
   {
   Property_form = new TFrequency_prop_form (this);
   }

// ------------------------------------------------------------------
//  Short description:
//      Setup the properties form.  This is called whenever the database
//      that is passed into this method is changed.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void Freq_child::Setup_properties (Database& DB)
   {
   TStringList* Field_list = new TStringList;
   Get_field_names (DB, Field_list);
   Property_form->Setup (Field_list);
   delete Field_list;
   }

// ------------------------------------------------------------------
//  Short description:
//      Show the properties form to the user.  Return true if user
//      pressed ok.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
bool Freq_child::Show_properties (void)
   {
   return (Property_form->ShowModal() == mrOk);
   }

// ------------------------------------------------------------------
//  Short description:
//      This method creates a chart object derived from High_level_chart
//      and simply returns a pointer to the caller.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
High_level_chart_base* Freq_child::Create_chart_object (void)
   {
   return new Frequency_chart(this);
   }

// ------------------------------------------------------------------
//  Short description:
//      Using the properties forms, this method creates xy plots on the
//      specified chart.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void Freq_child::Setup_chart_object (High_level_chart_base* Chart_p,
                                     bool Is_predicted)
   {
   Frequency_chart* Chart_ptr = dynamic_cast<Frequency_chart*>(Chart_p);
   if (Chart_ptr != NULL)
      {
      // create a format object to pass to chart.
      Format_base* Format_ptr;
      switch (Property_form->Chart_type_combo->ItemIndex)
         {
         case 0 : Format_ptr = new Scatter_format (Scatter_format::Markers);
                  break;
         case 1 : Format_ptr = new Scatter_format (Scatter_format::Markers_lines);
                  break;
         case 2 : Format_ptr = new Scatter_format (Scatter_format::Lines);
                  break;
         case 3 : Format_ptr = new Bar_format (Bar_format::Vertical_side_by_side);
                  break;
         case 4 : Format_ptr = new Bar_format (Bar_format::Vertical_stacked);
                  break;
         case 5 : Format_ptr = new Bar_format (Bar_format::Vertical_stacked100);
                  break;
         case 6 : Format_ptr = new Bar_format (Bar_format::Horizontal_side_by_side);
                  break;
         case 7 : Format_ptr = new Bar_format (Bar_format::Horizontal_stacked);
                  break;
         case 8 : Format_ptr = new Bar_format (Bar_format::Horizontal_stacked100);
                  break;
         }

      // loop through all selected items in x_variables listbox.
      for (int index = 0; index < Property_form->X_variables->Items->Count; index++)
         {
         if (Property_form->X_variables->Selected[index])
            Chart_ptr->Add_xy (Property_form->X_variables->Items->Strings[index].c_str(),
                               Format_ptr);
         }

      if (Property_form->Count->Checked)
         Chart_ptr->Frequency_type = Frequency_chart::count;
      else if (Property_form->Cum_prob->Checked)
         Chart_ptr->Frequency_type = Frequency_chart::prob_cum;
      else
         Chart_ptr->Frequency_type = Frequency_chart::prob_exceed;

      Chart_ptr->Start_point = atof(Property_form->Start_point_edit->Text.c_str());
      Chart_ptr->Interval = atof(Property_form->Interval_edit->Text.c_str());
      }
   }

