#pragma once

class Message;
// ---------------------------------------------------------------
// This class manages the creation of different types of messages.
// ---------------------------------------------------------------
class MessageType
	{
	public:
		enum Type
					{
					ActivateComponent = 1,
					AddComponent = 2,
					Checkpoint = 3,
					Commence = 4,
					Complete = 5,
					DeactivateComponent = 6,
					DeleteComponent = 7,
					Deregister = 8,
					Event = 9,
					GetValue = 10,
					Init1 = 11,
					Init2 = 12,
					NotifyAboutToDelete = 13,
					NotifyRegistrationChange = 14,
					NotifySetValueSuccess = 15,
					NotifyTermination = 16,
					PauseSimulation = 17,
					PublishEvent = 18,
					QueryInfo = 19,
					QuerySetValue = 20,
					QueryValue = 21,
					Register = 22,
					ReinstateCheckpoint = 23,
					ReplySetValueSuccess = 24,
					ReplyValue = 25,
					RequestComponentID = 26,
					RequestSetValue = 27,
					ResumeSimulation = 28,
					ReturnComponentID = 29,
					ReturnInfo = 30,
					ReturnValue = 31,
					TerminateSimulation = 32,

					ApsimGetQuery = 40,
					ApsimSetQuery = 41,
					ApsimChangeOrder = 42,
					};		
	
	
	};
