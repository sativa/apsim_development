#pragma once

#include <string>

// -------------------------------------
// This class handles all communications
// with other components.
// -------------------------------------
class Comms
	{
	public:
		Comms(void);
		~Comms(void);

		// ------------------------------------------------------
		// Event methods - Events include initialise and commence
		// ------------------------------------------------------
		void registerEventHandler( );
		void publishEvent( );

		// -------------------
		// Property methods
		// -------------------
		void registerProperty( );
		void getProperty( );
		void getProperties( );

		void registerSetHandler( );
		void setProperty( );

		// ---------------------------------------
		// Notify system of a warning.
		// Errors should be thrown.
		// ---------------------------------------
		void warning(const std::string& msg);

		// ---------------------
		// Write to summary file
		// ---------------------
		void writeToSummary( );

		// --------------------
		// Terminate simulation
		// --------------------
		void terminateSimulation(void);

	};
