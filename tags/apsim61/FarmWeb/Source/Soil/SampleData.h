//---------------------------------------------------------------------------

#ifndef SampleDataH
#define SampleDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ADODB.hpp>
#include <DB.hpp>
#include <vector>
#include <fstream>
class CharactData;
//---------------------------------------------------------------------------
// Class for encapsulating all access to the sample data
// in a database.
//---------------------------------------------------------------------------
class __declspec(dllexport) SampleData
   {
   private:
      const std::string fileName;
      CharactData* charactData;
      TADOConnection *db;
      TADOTable *farmerTable;
      TADOTable *farmTable;
      TADOTable *paddockTable;
      TADOTable *sampleTable;
      TADOTable *profileTable;
      TADOTable *profileDataTable;
      TADOTable *sampleDataTable;
      vector<double> depth;

      vector< vector<double> > wet;
      vector< vector<double> > dry;
      vector< vector<double> > nitrate;

      void outputToStream(std::ostream& out,
                          const AnsiString name,
                          const vector<double>& values,
                          const int numDecPlaces);

   public:
      SampleData(const std::string& fileName, CharactData* charactData);
      ~SampleData();

      // read all information based on current dataset pointers.
      void readInfo(void);


      vector<double> getDepth(void)  {return depth;}
      int getNumReps(void)           {return wet.size();}

      bool calcPAW(int Rep, vector<double>& PAW, vector<double>& targetDepth);
      bool calcTotalPAW (double& TotalPAW, vector<double>& targetDepth);

      bool calcVolumetric(int Rep, vector<double>& Volumetric, vector<double>& targetDepth);
      bool calcAverageVolumetric(vector<double>& Volumetric, vector<double>& targetDepth);

      bool calcNO3(int Rep, vector<double>& NO3, vector<double>& targetDepth);
      bool calcAverageNO3 (vector<double>& AverageN03, vector<double>& targetDepth);
      bool calcAverageNO3PPM (vector<double>& AverageN03, vector<double>& targetDepth);

      AnsiString calcAPSIMFile(void);

//      void saveTree(TGenericTree* Tree);
//      void restoreTree(TGenericTree* Tree);
//      AnsiString getTreePosition(void) {return savedTreePosition;}
//      void       setTreePosition(AnsiString position) {savedTreePosition = position;}

   };
#endif
