//---------------------------------------------------------------------------
#ifndef FarmwiseSequencerH
#define FarmwiseSequencerH
#include <ComponentInterface\Component.h>
#include <string>
#include <vector>

// Maximum number of layers in soil
#define maxEvents 100

// ------------------------------------------------------------------
// ------------------------------------------------------------------
class FarmwiseSequencer : public protocol::Component
   {
   public:
      FarmwiseSequencer(void);
      ~FarmwiseSequencer(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:
      void readConstants ( void );

      unsigned SubscribeEventsID[maxEvents];
      unsigned PublishEventsID[maxEvents][maxEvents];

      int numPublishEvents[maxEvents];
      int numSubscribeEvents;

      string SubscribeEvents[maxEvents];
      string PublishEvents[maxEvents][maxEvents];

      string cDebug;

   };

#endif
