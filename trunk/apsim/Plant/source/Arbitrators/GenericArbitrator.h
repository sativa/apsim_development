#ifndef GENERICARBITRATOR_H
#define GENERICARBITRATOR_H

class genericArbitrator : public Arbitrator
   {
  private:
   float frac_leaf[max_table];                       // fraction of remaining dm allocated to leaves
   float ratio_root_shoot[max_table];                // root:shoot ratio of new dm ()

  public:
   genericArbitrator(plantInterface *p) : Arbitrator(p) {};
   ~genericArbitrator(void) {};

   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void partitionDM(float,plantPart *,plantLeafPart *,plantPart *,plantPart *);
   virtual void zeroAllGlobals(void) ;
   virtual float dltDMWhole(float dlt_dm);
   };

#endif
