//---------------------------------------------------------------------------
#ifndef YPComponentH
#define YPComponentH
#include <ComponentInterface\Component.h>
#include <string>
#include <vector>
// ------------------------------------------------------------------
// Yield Prophet component.
// ------------------------------------------------------------------
class YPComponent : public protocol::Component
   {
   public:
      YPComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);

   private:
      unsigned cllID;
      unsigned dulID;
      unsigned swID;
      unsigned critSwID;
      unsigned rootDepthID;
      unsigned lldepID;
      unsigned duldepID;
      unsigned swdepID;
      unsigned dlayerID;

      std::vector<double> cll, dul, Depth;

      double interpFromArray(std::vector<double>& values);
      void getStaticVariables();
      std::vector<double> Accum(std::vector<double>& values);


   };


#endif
