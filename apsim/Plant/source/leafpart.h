#ifndef LeafPartH
#define LeafPartH

// Abstract class for leaf objects
class plantLeafPart : public plantPart {
  public:
   plantLeafPart(plantInterface *p, const string &name) : plantPart(p, name) {};
   virtual ~plantLeafPart() {};

   virtual void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue) =0;

   virtual void zeroAllGlobals(void) =0;
   virtual void zeroDeltas(void) =0;
   virtual void checkBounds(void) =0;
   virtual void doRegistrations(protocol::Component *) =0;
   virtual void readConstants (protocol::Component *, const string &) =0;
   virtual void readSpeciesParameters(protocol::Component *system, vector<string> &search_order) =0;
   virtual void onEmergence(void) =0;
   virtual void onKillStem(void) = 0;
   virtual float getLAI(void) const = 0;                             //
   virtual float getSLAI(void) const = 0;                            //
   virtual float getTLAI_dead(void) const = 0;                       //
   virtual float getLeafNo(void) const = 0;                          // The number of leaves 
   virtual float senFract (void) const = 0;                          // Fraction of canopy senescing today
   virtual float dmGreenDemand(void) const =0;                       // Maximum DM this part can take today

   virtual void potential (int option, float, float) =0;             // Calculate potentials
   virtual void leaf_area_stressed(float) =0;                        // the leaf area development from the
                                                                     //   potential daily increase in lai and stress factors
   virtual void actual(void) =0;                                     // Calculate actual leaf development from potential & stressed
   virtual void leaf_death (float nfact_expansion, float  dlt_tt) =0;// 
   virtual void leaf_area_sen (float, float) =0;
   virtual void detachment (void) =0;
   virtual void update (void) = 0;
   virtual void remove_detachment (float dlt_slai_detached, float dlt_lai_removed ) =0;

   virtual float dmRetransSupply(void) const;
   virtual void  doNConccentrationLimits(float modifier);
 
};


plantLeafPart* constructLeafPart(plantInterface *p, const string &type, const string &name);
#endif