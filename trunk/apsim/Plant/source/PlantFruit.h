
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef PLANTFRUIT_H
#define PLANTFRUIT_H

#ifndef __CSTRING_H
#include <cstring.h>
#endif

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

class PlantFruit
{
   friend ostream &operator<<(ostream &, const PlantFruit &);
	public:												// member functions
		PlantFruit(Plant *P);			// default constructor
//            PlantFruit(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
		PlantFruit(const PlantFruit &PlantFruit); 			// copy constructor
		const PlantFruit &operator=(const PlantFruit &other);		// Assigment operator

//            void setValue(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
            float coverTotal() const;
            float coverGreen() const;
            float coverSen() const;
            float coverDead() const;
            float interceptRadiation(float radiation);
//		float total() const;  	// Query

		virtual void display(ostream &os = cout) const;	// display function
            float calcCover (float cExtinctCoef, float pai);  // calc pod cover

            float divide (float dividend, float divisor, float default_value) const;  // Command

#if TEST_PlantFruit
		virtual ~PlantFruit();							// destructor
#else
		virtual ~PlantFruit();
#endif

//      float cover_green
//      float cover_sen
//      float cover_dead

      struct Cover
      {
         float green;
         float sen;
         float dead;
      };

      Cover cover;

//      struct PlantPartType
//         {
//         float leaf;
//         float stem;
//         };
//
//         PlantPartType green;
//         PlantPartType senesced;
//         PlantPartType dead;
	private:
      void read_constants (Plant *systemInterface);
      void zeroVariables();

      struct Constants
      {
         float extinctionCoeff;
      };
      Constants c;

      struct Globals
      {
         float pai;
      };
      Globals g;

      // The plant we hook into
      Plant *plant;
};

#endif
