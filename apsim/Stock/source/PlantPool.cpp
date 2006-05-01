
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#define YES 1
#define NO 0
#define TEST_PlantPool YES					// build unit test?
#include <vcl.h>
#include <math.h>

#ifndef PlantPool_H
#include "PlantPool.h"
#endif

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
// 	initialise data members.
// ----------------------------- PlantPartType ---------------------------------

PlantPartType::PlantPartType()  			 // member initialisation
{
   leaf = 0.0;
   stem = 0.0;
}

PlantPartType::PlantPartType(float leafInit, float stemInit)
{

	leaf = leafInit;
	stem = stemInit;
}

// destructor
PlantPartType::~PlantPartType()
{
}

ostream &operator<<(ostream &output, const PlantPartType &part)
{
	output << "   Leaf:     " << part.leaf << endl;
	output << "   Stem:     " << part.stem << endl << endl;
      return output;
}

// copy constructor
//	copy data members of object
PlantPartType::PlantPartType(const PlantPartType &PlantPartType)
{
 	leaf = PlantPartType.leaf;
	stem=PlantPartType.stem;
}


// Assigment operator
//	assign data members of object
const PlantPartType &PlantPartType::operator=(const PlantPartType &other)
{
	if (&other != this)			// don't self-assign
	{
		// copy data members over
            leaf = other.leaf;
            stem = other.stem;
      }
	return *this;
}

//===========================================================================
PlantPartType PlantPartType::operator+ (const PlantPartType &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPartType result;

   //Implementation
   result.leaf = leaf + y.leaf;
   result.stem = stem + y.stem;

   return result;
   }


//===========================================================================
PlantPartType PlantPartType::operator- (const PlantPartType &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPartType result;

   //Implementation
   result.leaf = leaf - y.leaf;
   result.stem = stem - y.stem;

   return result;
   }

// ----------------------------- PlantPool ---------------------------------

// default constructor
// 	initialise data members.
PlantPool::PlantPool()  			 // member initialisation
{
   green.leaf = 0.0;
   green.stem = 0.0;
   senesced.leaf = 0.0;
   senesced.stem = 0.0;
   dead.leaf = 0.0;
   dead.stem = 0.0;
}

PlantPool::PlantPool(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem)
{

	green.leaf = greenLeaf;
	green.stem = greenStem;
	senesced.leaf = senescedLeaf;
	senesced.stem = senescedStem;
	dead.leaf = deadLeaf;
	dead.stem = deadStem;
}

// destructor
PlantPool::~PlantPool()
{
}

ostream &operator<<(ostream &output, const PlantPool &pool)
{
	output << "PlantPool:" << endl;
	output << "   Green leaf:    " << pool.green.leaf << endl;
	output << "   Green stem:    " << pool.green.stem << endl;
	output << "   Senesced leaf: " << pool.senesced.leaf << endl;
	output << "   Senesced stem: " << pool.senesced.stem << endl;
	output << "   Dead leaf:     " << pool.dead.leaf << endl;
	output << "   Dead stem:     " << pool.dead.stem << endl << endl;
      return output;
}

// copy constructor
//	copy data members of object
PlantPool::PlantPool(const PlantPool &PlantPool)
{
 	green.leaf = PlantPool.green.leaf;
	green.stem=PlantPool.green.stem;
	senesced.leaf=PlantPool.senesced.leaf;
	senesced.stem=PlantPool.senesced.stem;
	dead.leaf=PlantPool.dead.leaf;
	dead.stem=PlantPool.dead.stem;
}


// Assigment operator
//	assign data members of object
const PlantPool &PlantPool::operator=(const PlantPool &other)
{
	if (&other != this)			// don't self-assign
	{
		// copy data members over
            green.leaf = other.green.leaf;
            green.stem = other.green.stem;
            senesced.leaf = other.senesced.leaf;
            senesced.stem = other.senesced.stem;
            dead.leaf = other.dead.leaf;
            dead.stem = other.dead.stem;
      }
	return *this;
}

//===========================================================================
PlantPool PlantPool::operator+ (const PlantPool &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool result;

   //Implementation
   result.green.leaf = green.leaf + y.green.leaf;
   result.green.stem = green.stem + y.green.stem;
   result.senesced.leaf = senesced.leaf + y.senesced.leaf;
   result.senesced.stem = senesced.stem + y.senesced.stem;
   result.dead.leaf = dead.leaf + y.dead.leaf;
   result.dead.stem = dead.stem + y.dead.stem;

   return result;
   }


//===========================================================================
PlantPool PlantPool::operator- (const PlantPool &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool result;

   //Implementation
   result.green.leaf = green.leaf - y.green.leaf;
   result.green.stem = green.stem - y.green.stem;
   result.senesced.leaf = senesced.leaf - y.senesced.leaf;
   result.senesced.stem = senesced.stem - y.senesced.stem;
   result.dead.leaf = dead.leaf - y.dead.leaf;
   result.dead.stem = dead.stem - y.dead.stem;

   return result;
   }

//===========================================================================
PlantPool PlantPool::operator* (const PlantPool &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool result;

   //Implementation
   result.green.leaf = green.leaf * y.green.leaf;
   result.green.stem = green.stem * y.green.stem;
   result.senesced.leaf = senesced.leaf * y.senesced.leaf;
   result.senesced.stem = senesced.stem * y.senesced.stem;
   result.dead.leaf = dead.leaf * y.dead.leaf;
   result.dead.stem = dead.stem * y.dead.stem;

   return result;
   }

//===========================================================================
PlantPool PlantPool::operator* (const float &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool result;

   //Implementation
   result.green.leaf = green.leaf * y;
   result.green.stem = green.stem * y;
   result.senesced.leaf = senesced.leaf * y;
   result.senesced.stem = senesced.stem * y;
   result.dead.leaf = dead.leaf * y;
   result.dead.stem = dead.stem * y;

   return result;
   }

//===========================================================================
PlantPool PlantPool::operator/ (const PlantPool &divisor)  const
//===========================================================================


   {
   //Constant Values

   //Local Varialbes
   PlantPool quotient;

   //Implementation
   quotient.green.leaf = divide(green.leaf, divisor.green.leaf, 0.0);
   quotient.green.stem = divide(green.stem, divisor.green.stem, 0.0);
   quotient.senesced.leaf = divide(senesced.leaf, divisor.senesced.leaf, 0.0);
   quotient.senesced.stem = divide(senesced.stem, divisor.senesced.stem, 0.0);
   quotient.dead.leaf = divide(dead.leaf, divisor.dead.leaf, 0.0);
   quotient.dead.stem = divide(dead.stem, divisor.dead.stem, 0.0);

   return quotient;
   }

//===========================================================================
PlantPool PlantPool::operator/ (const float &divisor)  const
//===========================================================================


   {
   //Constant Values

   //Local Varialbes
   PlantPool quotient;

   //Implementation
   quotient.green.leaf = divide(green.leaf, divisor, 0.0);
   quotient.green.stem = divide(green.stem, divisor, 0.0);
   quotient.senesced.leaf = divide(senesced.leaf, divisor, 0.0);
   quotient.senesced.stem = divide(senesced.stem, divisor, 0.0);
   quotient.dead.leaf = divide(dead.leaf, divisor, 0.0);
   quotient.dead.stem = divide(dead.stem, divisor, 0.0);

   return quotient;
   }


// Command
//===========================================================================
float PlantPool::divide (float dividend, float divisor, float default_value) const
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   const float LARGEST = 1.0e30;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   const float nought = 0.0;
   const float one = 1.0;
   const float granularity = 1.0e-6;

   //Local Varialbes
   float quotient;

   //Implementation
   if(floatsAreEqual(dividend, nought, granularity))      //multiplying by 0
      {
      quotient = nought;
      }
   else if(floatsAreEqual(divisor, nought, granularity))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < one)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > one)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }

//===========================================================================
PlantPool PlantPool::divide (PlantPool &dividend, PlantPool &divisor, float default_value)
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool quotient;

   //Implementation
   quotient.green.leaf = divide(dividend.green.leaf, divisor.green.leaf, default_value);
   quotient.green.stem = divide(dividend.green.stem, divisor.green.stem, default_value);
   quotient.senesced.leaf = divide(dividend.senesced.leaf, divisor.senesced.leaf, default_value);
   quotient.senesced.stem = divide(dividend.senesced.stem, divisor.senesced.stem, default_value);
   quotient.dead.leaf = divide(dividend.dead.leaf, divisor.dead.leaf, default_value);
   quotient.dead.stem = divide(dividend.dead.stem, divisor.dead.stem, default_value);

   return quotient;
   }

//===========================================================================
PlantPool PlantPool::multiply (const PlantPool &y)
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool result;

   //Implementation
   result.green.leaf = green.leaf * y.green.leaf;
   result.green.stem = green.stem * y.green.stem;
   result.senesced.leaf = senesced.leaf * y.senesced.leaf;
   result.senesced.stem = senesced.stem * y.senesced.stem;
   result.dead.leaf = dead.leaf * y.dead.leaf;
   result.dead.stem = dead.stem * y.dead.stem;

   return result;
   }

void PlantPool::setValue(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem)
{

	green.leaf = greenLeaf;
	green.stem = greenStem;
	senesced.leaf = senescedLeaf;
	senesced.stem = senescedStem;
	dead.leaf = deadLeaf;
	dead.stem = deadStem;
}

void PlantPool::setValue(PlantPartType greenValue, PlantPartType senescedValue, PlantPartType deadValue)
{

	green.leaf = greenValue.leaf;
	green.stem = greenValue.stem;
	senesced.leaf = senescedValue.leaf;
	senesced.stem = senescedValue.stem;
	dead.leaf = deadValue.leaf;
	dead.stem = deadValue.stem;
}



// Query
float PlantPool::total() const
{

	return green.leaf + green.stem + senesced.leaf + senesced.stem + dead.leaf + dead.stem;
}

void PlantPool::display(ostream &os) const
{
	os << "PlantPool: " << endl;
	os << "Green leaf: " << green.leaf << endl;
	os << "Green stem: " << green.stem << endl;
	os << "Senesced leaf: " << senesced.leaf << endl;
	os << "Senesced stem: " << senesced.stem << endl;
	os << "Dead leaf: " << dead.leaf << endl;
	os << "Dead stem: " << dead.stem << endl << endl;
}




#if TEST_PlantPool							// build unit test?


// PlantPool class test harness

// Tests default constructor, copy constructor, assignment operator and
// each of the get and set functions.  Does not test the destructor.

// Modification log
// 6 Aug 97  J. Hargreaves    Initial implementation


#ifndef PlantPool_H
#include "PlantPool.h"
#endif

int main()
{
	cout << "PlantPool test started" << endl;

	PlantPool p, *aPtr = &p;

	cout << endl << "Test set and get functions:" << endl;
	p.setValue(10.0, 2.0, 20.0, 3.0, 30.0, 4.0);
	if (p.total() != 69.0)
		cout << "setValue(10.0, 2.0, 20.0, 3.0, 30.0, 4.0) / Total() test FAILED"
		<< endl;

	cout << endl << "Test default constructor:" << endl;
	PlantPool q;                           						// run default constructor
	if (q.total() != 0.0)
		cout << "default constructor test FAILED" << endl;

	cout << endl << "Test constructor:" << endl;
	PlantPool a(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);                           						// run default constructor
	if (a.total() != 21.0)
		cout << "constructor test FAILED" << endl;

	cout << endl << "Test copy constructor:" << endl;
	PlantPool s = p;                       // run copy constructor
	if (s.total() != p.total())
      cout << "copy constructor test FAILED" << endl;

	cout << endl << "Test assignment operator:" << endl;
	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object

	if (s.total() != p.total())
	{
		s = p;                          // run operator=
		if (s.total() != p.total())
			cout << "assignment operator test FAILED" << endl;
	}
	else
		cout << "assignment operator test FAILED DIFFERENCE TEST" << endl;

	cout << endl << "Test multiply operator:" << endl;
	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
	PlantPool k = p * s;
	if (k.total() != 3856.0)
		cout << "multiply operator test FAILED" << endl;

	cout << endl << "Test simple multiply operator:" << endl;
	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
	 k = s * 2.0;
	if (k.total() != 396.0)
		cout << "simple multiply operator test FAILED" << endl;

	cout << endl << "Test divide operator:" << endl;
	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
	 k = s/p;
	if (k.total() < 16.58332 || k.total() > 16.58334)
		cout << "divide operator test FAILED" << endl;

	cout << endl << "Test simple divide operator:" << endl;
	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
	 k = s / 2.0;
	if (k.total() != 99.0)
		cout << "simple divide operator test FAILED" << endl;

	PlantPool t;
	t.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
	cout << endl << "Display PlantPool t" << endl;
	t.display();

	PlantPool x;
	x.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object

	cout << endl << "Display PlantPool x - static binding" << endl;
	x.display();

	cout << endl << "Display PlantPool x - dynamic binding" << endl;
	PlantPool *PlantPoolPtr = &x;
	PlantPoolPtr->display();

	cout << endl << "PlantPool test finished" << endl;
	return 0;
}

#endif

