#pragma once

namespace ComponentInterface {
ref class RegisteredEventHandlers
	{
	public:
		static unsigned add(IEventData* event) 
			{
			if (numEvents == MAX_NUM_EVENTS)
				throw gcnew Exception("Too many event handlers have been registered");
			events[numEvents] = event;
			numEvents++;
			return numEvents-1;
			}
		static IEventData* at(unsigned index)
			{
			if (index >= numEvents)
				throw gcnew Exception("Invalid event registeration id");
			return events[index];
			}
	
	private:
		static const unsigned MAX_NUM_EVENTS = 50;
		static IEventData**  events = new IEventData*[MAX_NUM_EVENTS];
		static unsigned numEvents = 0;
			
	};
};