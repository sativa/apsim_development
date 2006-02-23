//---------------------------------------------------------------------------

#ifndef UtilitiesH
#define UtilitiesH


#include <math.h>


#include "Conversions.h"


//class PlantInterface;

//------------------------------------------------------------------------------------------------


//      crop status names
typedef enum {out, dead, alive} Status;
//typedef enum {warning, fatal} errSeverity;
inline bool isEqual(float A, float B, float C) {return(fabs(A-B)<C);}
inline bool isEqual(float A, float B) {return(fabs(A-B)<1.0E-6);}

#define Max(a, b)  (((a) > (b)) ? (a) : (b))
#define Min(a, b)  (((a) < (b)) ? (a) : (b))

//------------------------------------------------------------------------------------------------
void checkRange(PlantInterface *p,float value, float lower, float upper,string msg);

bool readArray(PlantInterface *P,string section,string variable,string units,
                                             vector<string> &values,bool optional = false);
bool readArray(PlantInterface *P,string section,string variable,string units,
                     vector<float> &values,float lower,float upper,bool optional = false);
bool readArray(PlantInterface *P,vector<string> sections,string variable,vector<float> &values);
bool readVar(PlantInterface *P,string section,string variable,string units,string &value,
                                                                        bool optional = false);
bool readVar(PlantInterface *P,vector<string> sections,string variable,float &value,
                                                                        bool optional = false);
float readVar(PlantInterface *P,vector<string> sections,string variable);
bool readArray(PlantInterface *P,vector<string> sections,string variable,vector<int> &values);


int findIndex(float value, vector<float> items);
void convertVector(protocol::vector<float> &temp,vector<float> &newVect);
void fillVector(vector<float> &temp,vector<float> &newVect);
float layerProportion(vector<float> dLayer,float rootDepth,int rootLayer);
float sumVector(vector<float> vec);
float sumVector(vector<float> vec, int index);
float sumVector(vector<float> vec, int from, int to);
float divide (float dividend, float divisor, float default_value = 0.0);
float bound(float value,float lower, float upper);
float dayLength (int doy, float latitude, float twilight);

void accumulate (float value, vector<float> &array, float p_index, float dlt_index);
void calcPoolFractionDelta (int numParts, vector<float> fraction, vector<float> pool,
        vector<float> &dltPool);

void calcPartFractionDelta (int partNo, vector<float> fraction, float part,
      float &dltPart);


void JulianToCalendar(float jDay,int &day,int &month,int &year);
int CalendarToJulian(int day,int month,int year);

void summaryLine(PlantInterface *p,char *format,float arg0, float arg1);

//------------------------------------------------------------------------------------------------


class aDate
   {
   public:

   float julian;
   int year,month,day,doy;

   __fastcall aDate(float jDay){convertJulian(jDay);}

   aDate(){};
   void convertJulian(float jDay);
   };
//------------------------------------------------------------------------------------------------
class Today
   {
   public:
   
   aDate todayDate;
   float radn;                     // solar radiation (Mj/m^2/day)
   float minT;                     // minimum air temperature (oC)
   float maxT;                     // maximum air temperature (oC)
   float avgT;                     // average air temperature (oC)
   float rain;                     // rain in mm
   float vp;                       // VP
   float getPhotoPeriod(float latitude,float twilight){return dayLength (todayDate.doy,latitude,twilight);}
   };
//------------------------------------------------------------------------------------------------
// class to handle table functions

class TableFn
   {
   public:
   string xName,yName;
   vector<float> x;
   vector<float> y;

   TableFn(void){};
   TableFn(PlantInterface *P,  vector<string> sections, string xName, string yName);
   TableFn(vector<float> xVec,vector<float> yVec);
   void read(PlantInterface *P,  vector<string> sections, string xName, string yName);
   void load(vector<float> xVec,vector<float> yVec);
   float value(float v);
   };
//------------------------------------------------------------------------------------------------
#endif
