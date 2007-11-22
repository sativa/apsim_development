#ifndef BiomassH
#define BiomassH
class Biomass
   {
   public:
      Biomass();
      Biomass(float DM, float N, float P); 
      virtual void Clear();
      virtual float DM() const {return privateDM;}
      virtual float N()  const {return privateN;}
      virtual float P()  const {return privateP;}

      float Pconc() {return divide(P(),DM(),0.0);};
      float Nconc() {return divide(N(),DM(),0.0);};
      float NconcPercent() {return divide(N(),DM(),0.0)*fract2pcnt;};
      float PconcPercent() {return divide(P(),DM(),0.0)*fract2pcnt;};

      Biomass operator + (const Biomass& rhs);
      Biomass operator - (const Biomass& rhs);
      Biomass operator * (float Fraction);
      virtual Biomass& operator = (const Biomass& rhs);

   protected:
      virtual void CheckBounds();

   private:
      float privateDM;
      float privateN;
      float privateP;
};

#endif