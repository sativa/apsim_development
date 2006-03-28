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
      float svp(float temp); //(INPUT)  fraction of distance between svp at mi
      float vpd(float svp_fract, float maxt, float mint); //(INPUT)

      float divide (float dividend, float divisor, float default_value);

////      void doRunTimeReg(void);
////      void daylengthRelay (protocol::QueryValueData& queryData);

      unsigned sandID;
      unsigned vpdID;
      unsigned maxtID;
      unsigned mintID;

      string cDebug;
      int numLayers;
      float pSandLayer[100];
      float cSVPFract;
//   vector <double> sandLayers;

////      protocol::Component *system;
////      protocol::add_excretaType excreted;

   };

#endif
