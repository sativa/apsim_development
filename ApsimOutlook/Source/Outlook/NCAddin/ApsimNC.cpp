//---------------------------------------------------------------------------


#pragma hdrstop
                               
#include "ApsimNC.h"
#include "netcdfcpp.h"
#include "TCSV.h"
                                             
//---------------------------------------------------------------------------

#pragma package(smart_init)
//Public Interface **********************************************************
//---------------------------------------------------------------------------
//Description: Constructor - Creates the NetCDF object as a new NetCDF file with
//             the attributes passed to it
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
__fastcall ApsimNC::ApsimNC(TStringList *_Factors, TStringList **_Levels,
            TStringList *_Traits, String NCFileName)
   {
   //Set all globals to NULL
   nc = NULL;
   Factors = NULL;
   Levels = NULL;
   Traits = NULL;

   //Set up globals
   CurrentNCFile = NCFileName;
   SetAttributes(_Factors, _Levels, _Traits);

   //Create the NetCDF file
   if(!Create())
      {
      FreeMem();
      MessageDlg("Error creating NetCDF file '" + CurrentNCFile + "'",
         mtError, System::Set<TMsgDlgBtn,mbYes,mbHelp> () << mbOK, NULL);
      }
   }
//---------------------------------------------------------------------------
//Description: Constructor - Creates the NCFile object with an existing NetCDF file
//             and assigns the global attributes
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
__fastcall ApsimNC::ApsimNC(String NCFileName, bool ReadOnly)
   {
   //Set all globals to NULL
   nc = NULL;
   Factors = NULL;
   Levels = NULL;
   Traits = NULL;

   //Set up globals
   CurrentNCFile = NCFileName;
   if(ReadOnly)
      {
      nc = new NcFile(CurrentNCFile.c_str(), NcFile::ReadOnly);
      }
   else
      {
      nc = new NcFile(CurrentNCFile.c_str(), NcFile::Write);
      }


   if(!nc->is_valid())
      {
      FreeMem();
      MessageDlg("Error opening NetCDF file '" + CurrentNCFile + "'",
         mtError, System::Set<TMsgDlgBtn,mbYes,mbHelp> () << mbOK, NULL);
      }
   else
      {
      GetFileAttributes();
      }

   //Leave nc open in write mode
   }
//---------------------------------------------------------------------------
//Description: Destructor frees dynamic memmory
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
__fastcall ApsimNC::~ApsimNC(void)
   {
   //Free Memory
   FreeMem();
   }

//---------------------------------------------------------------------------
//Description: Returns the name of the NCFile currently open
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
String __fastcall ApsimNC::CurrentFile(void)
   {
   return CurrentNCFile;
   }
//---------------------------------------------------------------------------
//Description: Returns the number of factors including "Year"
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------

int __fastcall ApsimNC::NumFactors(void)
   {
   return Factors->Count;
   }
//---------------------------------------------------------------------------
//Description: Adds an APSIM output file to the NCFile
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
bool __fastcall ApsimNC::AddFile(String ApsFileName, String YearTrait)
   {
   //Local Vars
   TStringList *ApsFile = new TStringList;       //A TSL for holding the current
                                                 //Apsim file
   long *Coords = new long[Factors->Count];      //Array to store the coordintes
                                                 //of the factors from File
   long *Size  = new long[Factors->Count];       //Array to store the size of the
                                                 //matrix being loaded to nc
   int YearPos =                                 //The position of the year factor
         FindStringInTSL(Factors, "year");       //in the Factor TSL

   float **Data = new float* [Traits->Count];    //Matrix to hold the actual
                                                 //data as floats

   /* TODO :  */
   //Check if the YearTrait attribute has been added to the NCFile


   //Initialise Data
   for(int i = 0; i < Traits->Count; i++)
      {
      Data[i] = new float[Levels[YearPos]->Count];
      InitArray(Data[i], Levels[YearPos]->Count, -999.0);
      }

   //Load the Apsim file
   ApsFile->LoadFromFile(ApsFileName);

   //Get the factor coordinates and data size
   if(!GetFactorCoords(ApsFile, ApsFileName, Coords, Size))
      {
      //Free memory
      delete ApsFile;
      delete []Coords;
      delete []Size;
      for(int i = 0; i < Traits->Count; i++)
         {
         delete []Data[i];
         }
      delete []Data;
      return false;
      }

   //Get the data
   if(!ExtractApsData(ApsFile, ApsFileName, Data, YearPos, YearTrait))
      {
      //Free memory
      delete ApsFile;
      delete []Coords;
      delete []Size;
      for(int i = 0; i < Traits->Count; i++)
         {
         delete []Data[i];
         }
      delete []Data;
      return false;
      }

   NcVar **ncvar = new NcVar *[Traits->Count];

   //Put data into nc
   for(int i = 0; i < Traits->Count; i++)
      {
      ncvar[i] = nc->get_var(Traits->Strings[i].c_str());
      ncvar[i]->set_cur(Coords);
      ncvar[i]->put(Data[i],Size);
      }

   //Free memory
   delete ApsFile;
   delete []ncvar;
   delete []Coords;
   delete []Size;
   for(int i = 0; i < Traits->Count; i++)
      {
      delete []Data[i];
      }
   delete []Data;

   return true;
   }
//---------------------------------------------------------------------------
//Description: Assigns the NCFiles attributes to the TSL's passed in
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
void __fastcall ApsimNC::GetFileAttributes(TStringList *_Factors, TStringList **_Levels,
            TStringList *_Traits)
   {
   _Factors->Assign(Factors);

   for(int i = 0; i < Factors->Count; i++)
      {
      _Levels[i]->Assign(Levels[i]);
      }

   _Traits->Assign(Traits);

   return;
   }
//---------------------------------------------------------------------------
//Description: Returns the number of sites in the NCFile
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------

int __fastcall ApsimNC::NumSites(void)
   {
   //Find the position of the 'site' factor in Factors
   int SitePos = FindStringInTSL(Factors, "site");

   return Levels[SitePos]->Count;
   }
//---------------------------------------------------------------------------
//Description: Returns the number of years in the NCFile
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------

int __fastcall ApsimNC::NumYears(void)
   {
   //Find the position of the 'year' factor in Factors
   int YearPos = FindStringInTSL(Factors, "year");

   return Levels[YearPos]->Count;

   }
//---------------------------------------------------------------------------
//Description: Checks that a value has been recorded in a position. ie. if no
//             value has been recorded the co-ordinate will have the value of
//             NC_FILL_FLOAT
//Notes:       This function just checks the first site in the list
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------

bool __fastcall ApsimNC::CheckFirstDone(TStringList *Levs)
   {
   //Local Vars
   long *Coords;        //The NETCDF Coordinates
   long *Size;          //The NETCDF Size
   float *Data;         //The data returned from NETCDF file

   //Create dynamic mem
   Coords = new long[NumFactors()];
   Size = new long[NumFactors()];

   //Get the factor coords
   GetFactorCoords(Levs, Coords, Size);

   //Size should be either all '1' or all '1' except for the Number of years
   //which we'll get form size
   Data = new float[GetMaxVal((int*)Size, Factors->Count)];

   //Create the ncVar set and get data
   NcVar *Var = nc->get_var(Traits->Strings[0].c_str());
   Var->set_cur(Coords);
   Var->get(Data, Size);

   //Check it
   if(Data[0] == NC_FILL_FLOAT)
      {
      //Free memory
      delete [] Data;
      delete [] Coords;
      delete [] Size;
      return false;
      }
   //Free memory
   delete [] Data;
   delete [] Coords;
   delete [] Size;

   return true;
   }
//---------------------------------------------------------------------------
//Description: Return a list of the sites in the NCFile
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
bool __fastcall ApsimNC::GetSiteList(TStringList *Sites)
   {
   //Get the position of the 'site' factor in Factors
   int SitePos = FindStringInTSL(Factors, "site");

   if(SitePos == -1)
      {
      return false;
      }

   Sites->Clear();

   Sites->Assign(Levels[SitePos]);
   return true;
   }

//---------------------------------------------------------------------------
//Description: Checks that a value has been recorded in a position. ie. if no
//             value has been recorded the co-ordinate will have the value of
//             NC_FILL_FLOAT
//Notes:       This function checks all sites in the list
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
bool __fastcall ApsimNC::CheckAllDone(TStringList *Levs)
   {
    //Local Vars
   long *Coords;        //The NETCDF Coordinates
   long *Size;          //The NETCDF Size
   float *Data;         //The data returned from NETCDF file
   int SitePos =                            //Holds the position of the factor
         FindStringInTSL(Factors, "site");  //'site' within the site list

   //Create dynamic mem
   Coords = new long[NumFactors()];
   Size = new long[NumFactors()];

   //Get the factor coords
   GetFactorCoords(Levs, Coords, Size);

   //Size should be either all '1' or all '1' except for the Number of years
   //which we'll get form size
   Data = new float[GetMaxVal((int*)Size, Factors->Count)];

   //Create the ncVar set and get data
   NcVar *Var = nc->get_var(Traits->Strings[0].c_str());

   for(int i = 0; i < Levels[SitePos]->Count; i++)
      {
      Coords[SitePos] = i;
      Var->set_cur(Coords);
      Var->get(Data, Size);

      //Check it
      if(Data[0] == NC_FILL_FLOAT)
         {
         //Free memory
         delete [] Data;
         delete [] Coords;
         delete [] Size;
         return false;
         }
      }
   //Free memory
   delete [] Data;
   delete [] Coords;
   delete [] Size;

   return true;
   }

//---------------------------------------------------------------------------
//Description: Gets data for all sites in the site list for a particular trait
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------

bool __fastcall ApsimNC::GetAllData(TStringList *Facs, String Trait, float **Data)
   {

   int YearPos = FindStringInTSL(Factors, "year");    //The position of the year factor
                                                      //in the gloabal factors TSL
   int SitePos = FindStringInTSL(Factors, "site");    //The position of the year factor
                                                      //in the gloabal factors TSL
   int VarSitePos;                                    //The position of where the
                                                      //site factor in the current Facs TSL would be.
   TStringList *TSL = new TStringList;                //A construction TSL

   //Data wil be Sites x Years in dimension - How to check ?

   //Facs should be all of the factors except 'Site' and 'Year'
   if(Facs->Count != (Factors->Count - 2))
      {
      return false;
      }
   //Make TSL the same as Facs
   TSL->Assign(Facs);

   //Insert a site string into Facs
   if(SitePos < YearPos)
      {
      VarSitePos = SitePos;
      }
   else
      {
      VarSitePos = SitePos - 1;
      }
   TSL->Insert(VarSitePos, "Site");

   //Rename each site in the appropriate spot and get data
   for(int i = 0; i < Levels[SitePos]->Count; i++)
      {
      TSL->Strings[VarSitePos] = Levels[SitePos]->Strings[i];
      GetSiteData(TSL, Trait, Data[i]);
      }
   //Free memory
   delete TSL;

   return true;
   }
//---------------------------------------------------------------------------
//Description: Gets data for a particular site and trait
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------

bool __fastcall ApsimNC::GetSiteData(TStringList *Facs, String Trait, float *Data)
   {

   long *Coords = new long[Factors->Count];
   long *Size = new long[Factors->Count];

   GetFactorCoords(Facs, Coords, Size);

   NcVar *Var = nc->get_var(Trait.c_str());
   Var->set_cur(Coords);
   Var->get(Data, Size);

   //Free memory
   delete []Coords;
   delete []Size;

   return true;
}
//---------------------------------------------------------------------------
//Description: Gets the start year of the year list in the NCFile
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
int __fastcall ApsimNC::GetStartYear(void)
   {
   //Get the position of the year factor int he Factors TSL
   int YearPos = FindStringInTSL(Factors, "year");

   //Returns the first Year
   return Levels[YearPos]->Strings[0].ToInt();
   }
//---------------------------------------------------------------------------
//Description: Loads _Traits up with the Traits from the File
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
void ApsimNC::GetTraits(TStringList *_Traits)
   {
   for(int i = 0; i < Traits->Count; i++)
      {
      _Traits->Add(Traits->Strings[i]);
      }
   }

//Private Methods**********************************************************
//---------------------------------------------------------------------------
//Description: Sets the NCFiles Attributes
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
void __fastcall ApsimNC::SetAttributes(TStringList *_Factors, TStringList **_Levels,
      TStringList *_Traits)
   {
   //Assign
   Factors = new TStringList;
   Factors->Assign(_Factors);

   Levels = new TStringList *[Factors->Count];
   for(int i = 0; i < Factors->Count; i++)
      {
      Levels[i] = new TStringList;
      Levels[i]->Assign(_Levels[i]);
      }
   Traits = new TStringList;
   Traits->Assign(_Traits);
   }
//---------------------------------------------------------------------------
//Description: Creates a comma separated string of the contents of the TSL
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
String __fastcall ApsimNC::CreateListString(TStringList *List)
   {
   return List->CommaText;

   /*//Local Vars
   int i;

   String Str = "";
   for(i = 0 ;i < (List->Count - 1); i++)
      {
      Str += List->Strings[i] + ",";
      }
   //Add the last one without a comma
   Str += List->Strings[i];

   return Str;*/
   }

//---------------------------------------------------------------------------
//Description: Deletes Global dynamic memory
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
void __fastcall ApsimNC::FreeMem()
   {
   //Free memory
   if(nc)
      {
      nc->close();
      delete nc;
      nc = NULL;
      }
   if(Factors)
      {
      delete Factors;
      Factors = NULL;
      }
   if(Levels)
      {
      delete []Levels;
      Levels = NULL;
      }
   if(Traits)
      {
      delete Traits;
      }
   }

//---------------------------------------------------------------------------
//Description: Extracts the data from an APSIM output file
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
bool __fastcall ApsimNC::ExtractApsData(TStringList *File, String FileName, float **Data,
      int YearPos, String YearTrait)
   {
   //Local vars
   TStringList *ThisTraits = new TStringList;    //TSL for holding the traits of
                                                 //the current file
   TStringList *Line = new TStringList;          //TSL to hold the individual
                                                 //lines of file
   int *TraitCoords;                             //Array to hold the trait coords
                                                 //of the current file
   int Pos;                                      //The position of the Traits line
                                                 //within the file
   int YearTraitPos;                             //The position of the Year trait in the
                                                 //Trait list from the current file.
                                                 //This will be used to index the Data
                                                 //array

   //Get the traitlist
   ThisTraits->CommaText = GetTraits(File, FileName, Pos);

   //Check the file is OK
   if(Pos == -1)
      {
      //Pretty unlikely
      //Free memory
      delete ThisTraits;
      delete Line;
      return false;
      }

   //Make a trait coordinate array
   TraitCoords = new int[ThisTraits->Count];

   //Initialize array
   InitArray(TraitCoords, ThisTraits->Count, -1);

   //Find the position of the year trait
   YearTraitPos = FindStringInTSL(ThisTraits, YearTrait);

   //Get the coordinates of the 'used' traits ie not all traits from File need
   //to be in Traits
   for(int i = 0; i < ThisTraits->Count; i++)
      {
      for(int j = 0; j < Traits->Count; j++)
         {
         if(ThisTraits->Strings[i] == Traits->Strings[j])
            {
            TraitCoords[i] = j;
            break;
            }
         }
      }

   //Set the initial 'data' Line - Assuming Apsim data starts 2 lines after the
   //trait line

   //Get the data
   for(int i = (Pos + 2); i < File->Count; i++)
      {
      Line->CommaText = File->Strings[i];
      for(int j = 0; j < Line->Count; j++)
         {
         if(TraitCoords[j] > -1)
            {
            //Fill Data if the current file trait is include in the global traits list
            int Index = int(Line->Strings[YearTraitPos].ToDouble()) -
                     int(Levels[YearPos]->Strings[0].ToDouble());
            Data[TraitCoords[j]][Index] = float(Line->Strings[j].ToDouble());
            }
         }
      }
   //Free memory
   delete ThisTraits;
   delete Line;
   delete [] TraitCoords;
   return true;
   }
//---------------------------------------------------------------------------
//Description: Calculates the coordinates of a certain combination of levels
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------

bool __fastcall ApsimNC::GetFactorCoords(TStringList *Levs, long *Coords, long *Size)
{
   int YearPos =                                 //The position of the year factor
         FindStringInTSL(Factors, "year");       //in the Factor TSL
   int SitePos =                                 //The position of the site factor
         FindStringInTSL(Factors, "site");       //in the Factor TSL

   int LevelPos = 0;                             //The current position int the TSL Levs

   //Initialise the coords for later checking
   InitArray((int*)Coords, Factors->Count, -1);

   //Init the size array - it will be size of 1 for all coordinates except year-
   //which will be NumYears which will be adjusted later
   InitArray((int*)Size, Factors->Count, 1);

   /* TODO :  */
   //Should have some check on correctness of factors

   //Assumptions: * If Levs->Count == Factors->Count then Levs corresponds to a unique
   //               Factor combination at a particular site at a particular year ie a
   //               scalar.
   //             * If Levs->Count == (Factors->Count - 1) then the levs correspond
   //               to all factors except 'Year' ie a unique factor combination for a
   //               particular site
   //             * If Levs->Count == (Factors->Count - 2) then levs correspond to all
   //               factors except 'Site' and 'Year' ie. a unique factor combination
   //               at any site

   if((Factors->Count - Levs->Count) == 0)
      {
      //Get the Level coords for this TSL
      for(int i = 0; i < Levs->Count; i++)
         {
         Coords[i] = FindStringInTSL(Levels[i], Levs->Strings[i]);
         }
      //Note: All size[] is 1 as it is looking for a scalar
      }
   else if(((Factors->Count - Levs->Count) == 1) ||
             ((Factors->Count - Levs->Count) == 2))
      {
      //Get the Level coords for this TSL
      for(int i = 0; i < Factors->Count; i++)
         {
         if(i != YearPos && i != SitePos)
            {
            Coords[i] = FindStringInTSL(Levels[i], Levs->Strings[LevelPos]);
            LevelPos++;
            }
         else if(i == SitePos)
            {
            if((Factors->Count - Levs->Count) == 1) //Site included
               {
               Coords[i] = FindStringInTSL(Levels[i], Levs->Strings[LevelPos]);
               LevelPos++;
               }
            else     //No site included
               {
               Coords[i] = 0;    //Default to the first site
               }
            }
         else  //(i == YearPos)
            {
            //Set the year coord to '0' because thats where we want
            //to start the array
            Coords[i] = 0;
            //Also set the size array's year part
            Size[i] = Levels[i]->Count;
            }
         }
      }
   else    //All other cases
      {
      return false;
      }

   //Check that all coords were found ie. all are valid levels
   for(int i = 0; i < Factors->Count; i++)
      {
      if(Coords[i] == -1)
         {
         return false;
         }
      }
   return true;
   }

//---------------------------------------------------------------------------
//Description: Gets the coordinates of a certain combination of Levels
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
bool __fastcall ApsimNC::GetFactorCoords(TStringList *File, String FileName, long *Coords,
   long *Size)
   {
   //Local Vars
   TStringList *ThisFacs = new TStringList;      //A TSL for holding the factors
                                                 //from the current file
   TStringList *ThisLevs = new TStringList;      //A TSL for holding the levels
                                                 //from the current file

   //Get the title from the TSL
   String Title = GetTitle(File, FileName);

   //Parse the title
   ParseTitle(Title, ThisFacs, ThisLevs);

   //Check that all coords were found ie. all are valid levels
   if(!GetFactorCoords(ThisLevs, Coords, Size))
      {
      MessageDlg("The File '" + FileName + "' does not have the correct Levels.!\nIgnoring",
         mtWarning, System::Set<TMsgDlgBtn,mbYes,mbHelp> () << mbOK, NULL);
      //Free memory
      delete ThisFacs;
      delete ThisLevs;
      return false;
      }

   //Free memory
   delete ThisFacs;
   delete ThisLevs;

   return true;
   }

//---------------------------------------------------------------------------
//Description: Gets the title from an APSIM output file
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
String __fastcall ApsimNC::GetTitle(TStringList *File, String FileName)
   {
   int i;            // A counter
   for(i = 0; i < File->Count; i++)
      {
      if(File->Strings[i].UpperCase().Pos("TITLE"))
         {
         break;
         }
      }

   //A check to see if the title was found
   if(i == File->Count)
      {
      MessageDlg("The file '" + FileName +"' Does not appear to be a valid Apsim output file.",
         mtError, System::Set<TMsgDlgBtn,mbYes,mbHelp> () << mbOK, NULL);
      return "";
      }

   //On success
   return File->Strings[i];
   }
//---------------------------------------------------------------------------
//Description: Parses an APSIM output file title
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
void __fastcall ApsimNC::ParseTitle(String Title, TStringList *Facs, TStringList *Levs)
   {

   //TStringList *Params = new TStringList;
   TCSV *Params = new TCSV;

   //If title ends in ";" get rid of it
   if(*Title.AnsiLastChar() == ';')
      {
      Title.SetLength(Title.Length() - 1);
      }

   //Replace characters ' ; ' with ','
   Title = StringReplace(Title, " ; ", "," , TReplaceFlags() << rfReplaceAll);

   //Replace characters '; ' with ','
   Title = StringReplace(Title, "; ", "," , TReplaceFlags() << rfReplaceAll);

   //Replace characters ';' with ','
   Title = StringReplace(Title, " ;", "," , TReplaceFlags() << rfReplaceAll);

   //Replace characters ' = ' with ','
   Title = StringReplace(Title, " = ", "=" , TReplaceFlags() << rfReplaceAll);

   //Replace characters '= ' with ','
   Title = StringReplace(Title, "= ", "=" , TReplaceFlags() << rfReplaceAll);

   //Replace characters ' =' with ','
   Title = StringReplace(Title, " =", "=" , TReplaceFlags() << rfReplaceAll);

   //Find the position of the first '='; Should be of the form Title= .....
   int pos = Title.Pos("=");

   //Get rid of the string 'Title='. This will leave Title as name=value
   //pairs for the factors of the simulation
   Title = Title.SubString((pos + 1), Title.Length() - pos).Trim();

   //Set the CommaText for the StringList
   Params->CommaText = Title;

   Facs->Clear();
   if(Levs)
      {
      Levs->Clear();
      }
   //Extract factors and levels
   for(int i = 0; i < Params->Count; i++)
      {
      Facs->Add(Params->Names[i]);
      if(Levs)
         {
         Levs->Add(Params->Values[Params->Names[i]]);
         }
      }

   //Free Memory
   delete Params;

   }
//---------------------------------------------------------------------------
//Description: Gets the Trait names from an APSIM output file
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
String __fastcall ApsimNC::GetTraits(TStringList *File, String FileName,
      int &Index)
   {
   for(Index = 0; Index < File->Count; Index++)
      {
      //The first line without an '=' should be the trait list
      if(!File->Strings[Index].Pos("="))
         {
         break;
         }
      }

   //A check to see if the traits were found
   if(Index == File->Count)
      {
      Index = -1;
      MessageDlg("The file '" + FileName +"' Does not appear to be a valid Apsim output file.",
         mtError, System::Set<TMsgDlgBtn,mbYes,mbHelp> () << mbOK, NULL);
      return "";
      }

   return File->Strings[Index];
   }

//---------------------------------------------------------------------------
//Description: Finds the index of a String in a TSL
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
int __fastcall ApsimNC::FindStringInTSL(TStringList *TSL, String Var)
   {
   for(int i = 0; i < TSL->Count; i++)
      {
      if(TSL->Strings[i].UpperCase() == Var.UpperCase())
         {
         return i;
         }
      }

   return -1;
   }
//---------------------------------------------------------------------------
//Description: Initialises an  array of floats to 'num'
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
void __fastcall ApsimNC::InitArray(float *Arr, int Count, float Num)
   {
   for(int i = 0; i < Count; i++)
      {
      Arr[i] = Num;
      }
   }
//---------------------------------------------------------------------------
//Description: Initialises an array of ints to 'num'
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
void __fastcall ApsimNC::InitArray(int *Arr, int Count, int Num)
   {
   for(int i = 0; i < Count; i++)
      {
      Arr[i] = Num;
      }
   }
//---------------------------------------------------------------------------
//Description: Gets an existing NCFiles attributes and stores them in the global
//             containers
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
bool __fastcall ApsimNC::GetFileAttributes()
   {

   TCSV *FileFactors = new TCSV;
   TCSV *FileTraits = new TCSV;
   TCSV **FileLevels;

   //Get the factor list
   NcAtt *Att = nc->get_att("FactorList");
   FileFactors->CommaText = String(Att->as_string(0));

   //Get the trait list
   Att = nc->get_att("TraitList");
   FileTraits->CommaText = String(Att->as_string(0));



   //Set up Levels
   FileLevels = new TCSV* [FileFactors->Count];
   for(int i = 0; i < FileFactors->Count; i++)
      {
      FileLevels[i] = new TCSV;
      }

   //Get the level list
   for(int i =0; i < FileFactors->Count; i++)
      {
      NcVar *Var = nc->get_var(FileFactors->Strings[i].c_str());
      NcAtt *VarAtt = Var->get_att("LevelList");
      FileLevels[i]->CommaText = String(VarAtt->as_string(0));
      }

   //Set up the global TSL's
   Factors = new TStringList;
   Factors->Assign(FileFactors);

   Traits = new TStringList;
   Traits->Assign(FileTraits);

   //Set up Levels
   Levels = new TStringList* [Factors->Count];
   for(int i = 0; i < Factors->Count; i++)
      {
      Levels[i] = new TStringList;
      Levels[i]->Assign(FileLevels[i]);
      }

   //Free Memory
   delete FileFactors;
   delete FileTraits;
   delete []FileLevels;
   
   return true;
   }

//---------------------------------------------------------------------------
//Description: Gets the maximum value from an array of ints
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
int __fastcall ApsimNC::GetMaxVal(int *Arr, int Size)
   {
   int MaxVal = 0;
   for(int i = 0; i < Size; i++)
      {
      if(Arr[i] > MaxVal)
         {
         MaxVal = Arr[i];
         }
      }
   return MaxVal;
   }
//---------------------------------------------------------------------------
//Description: Creates an new NetCDF file using attributes stored in the global
//             attribute containers
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
bool __fastcall ApsimNC::Create(void)
   {

   //Create the NcFile and leave in define mode
   nc = new NcFile(CurrentNCFile.c_str(), NcFile::Replace);

   // Check if the file was opened correctly
   if (!nc->is_valid())
      {
	   ShowMessage("Can't create netCDF file " + CurrentNCFile);
      return false;
      }

   // Create dimensions we will not have any unlimited
    NcDim **Dims = new NcDim* [Factors->Count];
    for(int i = 0; i < Factors->Count; i++)
      {
      Dims[i] = nc->add_dim(Factors->Strings[i].c_str(), Levels[i]->Count);
      }

   // Global attributes
   nc->add_att("FactorList", CreateListString(Factors).c_str());
   nc->add_att("TraitList", CreateListString(Traits).c_str());

   // Create Coordinate variables and their attributes
   NcVar **CVars = new NcVar* [Factors->Count];
   for(int i = 0; i < Factors->Count; i++)
      {
      CVars[i] = nc->add_var(Factors->Strings[i].c_str(), ncInt, Dims[i]);
      CVars[i]->add_att("LevelList", CreateListString(Levels[i]).c_str());
      String Range = "0 to " + String(Levels[i]->Count - 1);
      CVars[i]->add_att("IndexedFrom",Range.c_str());
      }

   // Create Data variables and their attributes
   NcVar **DVars = new NcVar* [Traits->Count];
   for(int i = 0; i < Traits->Count; i++)
      {
      DVars[i] = nc->add_var(Traits->Strings[i].c_str(),ncFloat,Factors->Count,Dims );
      Application->ProcessMessages();
      }

   //Delete the create nc and leave open for write
   if(nc)
      {
      delete nc;
      nc = NULL;
      }
   nc = new NcFile(CurrentNCFile.c_str(), NcFile::Write);

   //Free memory
   delete []Dims;
   delete []CVars;
   delete []DVars;

   return true;
   }
//---------------------------------------------------------------------------
//Description: Returns true if the factor level combination exists
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------

bool __fastcall ApsimNC::CheckScenario(TStringList *CheckFacs, TStringList *CheckLevs)
   {
   TStringList *TestFactors = new TStringList;
   TStringList **TestLevs = new TStringList *[Factors->Count - 1]; //-1 because the year factor is being stripped

   for(int i = 0; i < (Factors->Count - 1); i++)
      {
      TestLevs[i] = new TStringList;
      }

   int YearIndex = FindStringInTSL(Factors, "year");

   for(int i = 0; i < Factors->Count; i++)
      {
      if(i != YearIndex)
         {
         TestFactors->Add(Factors->Strings[i]);
         TestLevs[i]->Assign(Levels[i]);
         }
      }

   //Now Test

   //Firstly size
   if(TestFactors->Count != CheckFacs->Count)
      {
      //free memory
      delete TestFactors;
      delete []TestLevs;

      return false;
      }
   //Secondly check factors
   for(int i = 0; i < TestFactors->Count; i++)
      {
      if(CheckFacs->Strings[i] != TestFactors->Strings[i])
         {
         //free memory
         delete TestFactors;
         delete []TestLevs;

         return false;
         }
      }

   //Thirdly levels
   for(int i = 0; i < CheckLevs->Count; i++)
      {
      if(FindStringInTSL(TestLevs[i], CheckLevs->Strings[i]) == -1)
         {
         //free memory
         delete TestFactors;
         delete []TestLevs;

         return false;
         }
      }
   //free memory
   delete TestFactors;
   delete []TestLevs;

   //if it gets to here
   return true;
   }
