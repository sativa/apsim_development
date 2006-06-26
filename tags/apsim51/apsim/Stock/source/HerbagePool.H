
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#ifndef HerbagePool_H
#define HerbagePool_H

#ifndef __CSTRING_H
#include <cstring.h>
#endif

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

class HerbagePartType
{
   friend ostream &operator<<(ostream &, const HerbagePartType &);
   public:
      HerbagePartType();
      virtual ~HerbagePartType();							// destructor
      HerbagePartType(float leafInit, float stemInit);
	HerbagePartType(const HerbagePartType &HerbagePartType); 						// copy constructor
	const HerbagePartType &operator=(const HerbagePartType &other);		// Assigment operator

      HerbagePartType operator- (const HerbagePartType &y) const;  // Command
      HerbagePartType operator+ (const HerbagePartType &y) const;  // Command

      float leaf;
      float stem;

   private:
};

class HerbagePool
{
   friend ostream &operator<<(ostream &, const HerbagePool &);
	public:												// member functions
		HerbagePool();			// default constructor
            HerbagePool(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
		virtual ~HerbagePool();							// destructor
		HerbagePool(const HerbagePool &HerbagePool); 						// copy constructor
		const HerbagePool &operator=(const HerbagePool &other);		// Assigment operator

            HerbagePool operator- (const HerbagePool &y) const;  // Command
            HerbagePool operator+ (const HerbagePool &y) const;  // Command
            HerbagePool operator* (const HerbagePool &y) const;  // Command
            HerbagePool operator* (const float y) const;  // Command
            HerbagePool operator/ (const HerbagePool &y) const;  // Command
            HerbagePool operator/ (const float y) const;  // Command
            HerbagePool multiply (const HerbagePool &y);  // Command
            HerbagePool divide (HerbagePool &dividend, HerbagePool &divisor, float default_value);  // Command
            float divide (float dividend, float divisor, float default_value) const;  // Command
            void setValue(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
            void setValue(HerbagePartType green, HerbagePartType senesced, HerbagePartType dead);
		float total() const;  	// Query

		virtual void display(ostream &os = cout) const;	// display function



         HerbagePartType green;
         HerbagePartType senesced;
         HerbagePartType dead;
	private:
};

#endif
