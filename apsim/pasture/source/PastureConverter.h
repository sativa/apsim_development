//---------------------------------------------------------------------------
#ifndef PastureConverterH
#define PastureConverterH
#include <ComponentInterface\Component.h>
#include <string>
#include <vector>

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
// Maximum number of layers in soil
#define max_layer 100

// ------------------------------------------------------------------
// ------------------------------------------------------------------
class PastureConverter : public protocol::Component
   {
   public:
      PastureConverter(void);
      ~PastureConverter(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:
      void readParameters ( void );
      void sendSand (protocol::QueryValueData& queryData);
      void sendVPD (protocol::QueryValueData& queryData);
      void sendWeather (protocol::QueryValueData& queryData);
      void sendSWLayer (protocol::QueryValueData& queryData);
      void sendNH4 (protocol::QueryValueData& queryData);
      void sendNO3 (protocol::QueryValueData& queryData);
      float svp(float temp); //(INPUT)  fraction of distance between svp at mi
      float vpd(float svp_fract, float maxt, float mint); //(INPUT)
      void doPrepare(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doProcess(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doPost(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doCropWaterUptake(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doCropNutrientUptake(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doAddFOM(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void dosowPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void doNewProfile(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

      float divide (float dividend, float divisor, float default_value);

////      void doRunTimeReg(void);
////      void daylengthRelay (protocol::QueryValueData& queryData);

      unsigned prepareID;
      unsigned processID;
      unsigned postID;
      unsigned initStepID;
      unsigned doPastureWaterID;
      unsigned doPastureGrowthID;
      unsigned preWaterBalanceID;
      unsigned endStepID;
      unsigned sandID;
      unsigned vpdID;
      unsigned maxtID;
      unsigned mintID;
      unsigned rainID;
      unsigned snowID;
      unsigned radnID;
      unsigned windID;
      unsigned fomAddedID;
      unsigned onUptakeID;
      unsigned incorpFOMID;
      unsigned cropwatersupplyID;
      unsigned cropwaterdemandID;
      unsigned rtDepID;
      unsigned waterInfoID;
      unsigned ll15DepthID;
      unsigned swID;
      unsigned swDepthID;
      unsigned swLayerID;
      unsigned dltSWDepthID;
      unsigned dltNO3ID;
      unsigned dltNH4ID;
      unsigned dltPOxID;
      unsigned dltSO4ID;
      unsigned killID;
      unsigned cutID;
      unsigned cultivateID;
      unsigned sowID;
      unsigned sowPastureID;
      unsigned dlayerID;
      unsigned nh4ppmID;
      unsigned nh4_ppmID;
      unsigned no3ppmID;
      unsigned no3_ppmID;
      unsigned weatherID;
      unsigned newProfileID;

      string cDebug;
      int numLayers;
      vector <float> pSandLayer;
//      float pSandLayer[100];

      float dlayer [max_layer];                         // thickness of soil layer I (mm)
      float dlt_sw_dep[max_layer];                      // water uptake in each layer (mm water)
      float ll15_dep[max_layer];
      float dul_dep [max_layer];                        // drained upper limit soil water content for soil layer L (mm water)
      float sat_dep[max_layer];
      float bd[max_layer];
      float sw_dep [max_layer];                         // soil water content of layer L (mm)

      float cSVPFract;
      protocol::pasturewatersupplyType waterSupply;


//   vector <double> sandLayers;

////      protocol::Component *system;
////      protocol::add_excretaType excreted;

   };

#endif
